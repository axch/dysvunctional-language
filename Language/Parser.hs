{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Parser where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Token
import FOL.Language.Pretty (pprint)
import FOL.Language.Rename (rename)
import FOL.Language.Unique

import Control.Applicative
import Control.Arrow

import Data.Maybe

import Text.Parsec.Prim hiding (many, (<|>), parse)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Text.ParserCombinators.Parsec.Combinator (between, manyTill)

-- A custom parser type that accepts streams of 'Token's.
type Parser = GenParser Token ()

-- Parser combinators

-- Accepts a token 't' with the result 'x' when 'test t' is 'Just x'.
-- Like 'token' from Text.Parsec.Prim, but uses the default token
-- pretty-printing function and ignores the position info.
tok :: (Token -> Maybe a) -> Parser a
tok test = tokenPrim show (\pos _ _ -> pos) test

-- Accepts the token 't' with the result 't'.
lit :: Token -> Parser Token
lit t = tok maybeLiteral
    where
      maybeLiteral x
          | x == t    = Just x
          | otherwise = Nothing

-- Accept a token 't' with the result 'n' when 't' is the symbol 'n'.
sym :: Parser String
sym = tok maybeSymbol
    where
      maybeSymbol (TokSymbol x) = Just x
      maybeSymbol _             = Nothing

-- Parse a symbol, try to match it agains a list of alternatives, and
-- if a match is found, apply the corresponding parser to the rest of
-- the input.
caseParser :: [(String, Parser a)] -> Parser a
caseParser alternatives = fromMaybe empty . flip lookup alternatives =<< sym

-- Literal parens.
lparen, rparen :: Parser Token
lparen = lit TokLParen
rparen = lit TokRParen

-- Accepts a token 't' with the result 'n' when 't' is the symbol 'n'
-- that is not a reserved keyword.
identifier :: Parser Name
identifier = tok maybeIdentifier
    where
      maybeIdentifier (TokSymbol x)
          | x `notElem` reserved = Just (Name x)
          | otherwise            = Nothing
      maybeIdentifier _          = Nothing

-- Accepts a token 't' with the result 'd' when 't' is the real 'd'.
real :: Parser Real
real = tok maybeReal
    where
      maybeReal (TokReal r) = Just r
      maybeReal _           = Nothing

-- To parse 'vector-ref's we must be able to parse integers, but I am
-- lazy to modify the tokenizer.  Whenever an integer is expected, it
-- is parsed as a real and is truncated.
integer :: Parser Integer
integer = truncate <$> real

-- Helper combinators and aliases for better readability.

parens :: Parser a -> Parser a
parens = between lparen rparen

listOf :: Parser a -> Parser [a]
listOf p = parens (many p)

tagged :: String -> Parser a -> Parser a
tagged name body = parens (lit (TokSymbol name) >> body)

-- Empty list can occur both as expressions and as types, so it makes
-- sense to abstract the common bit.
parseEmptyList :: Parser ()
parseEmptyList = do { lparen; rparen; return () }

-- Parsing types

parseNilTy, parseConsTy, parseVectorTy, parseValuesTy, parseType :: Parser Type
parseNilTy    = NilTy <$ parseEmptyList
parseConsTy   = liftA2 ConsTy parseType parseType
parseVectorTy = liftA  VectorTy (many parseType)
parseValuesTy = liftA  ValuesTy (many parseType)

parseType = try parseNilTy <|> parseAtomicType <|> parens parseCompoundType
    where
      parseAtomicType   = caseParser [ ("real",   return RealTy)
                                     , ("bool",   return BoolTy)
                                     ]
      parseCompoundType = caseParser [ ("cons",   parseConsTy  )
                                     , ("vector", parseVectorTy)
                                     , ("values", parseValuesTy)
                                     ]

-- Parsing programs

parseNil :: Parser (Expr Name)
parseNil = Nil <$ parseEmptyList

parseVariable :: Parser (Expr Name)
parseVariable = Var <$> identifier

parseConstant :: Parser (Expr Name)
parseConstant = try parseNil <|> tok maybeConstant
    where
      maybeConstant (TokBool b) = Just (Bool b)
      maybeConstant (TokReal r) = Just (Real r)
      maybeConstant _           = Nothing

-- The list of reserved keywords.
reserved :: [String]
reserved = [ "begin"
           , "define"
           , "if"
           , "let"
           , "let-values"
           , "cons"
           , "vector"
           , "values"
           , "car"
           , "cdr"
           , "vector-ref"
           ]

parseDefine :: Parser (Defn Name)
parseDefine = tagged "define" $ do
                (proc, args) <- parens $ liftA2 (,) identifier (many identifier)
                types <- tagged "argument-types" (many parseType)
                let proc_type = last types
                    arg_types = init types
                body <- parseExpression
                return $ Defn (proc, proc_type) (zip args arg_types) body

parseIf, parseLet, parseLetValues, parseCar, parseCdr :: Parser (Expr Name)
parseVectorRef, parseCons, parseVector, parseValues   :: Parser (Expr Name)

parseIf = liftA3 If predicate consequent alternate
    where
      predicate  = parseExpression
      consequent = parseExpression
      alternate  = parseExpression

parseLet = liftA2 Let bindings body
    where
      bindings = listOf binding
      binding  = parens $ liftA2 (,) pattern expr
      pattern  = identifier
      expr     = parseExpression
      body     = parseExpression

parseLetValues = liftA2 LetValues bindings body
    where
      bindings = listOf binding
      binding  = parens $ liftA2 (,) pattern expr
      pattern  = listOf identifier
      expr     = parseExpression
      body     = parseExpression

parseCar       = liftA  Car       parseExpression
parseCdr       = liftA  Cdr       parseExpression
parseVectorRef = liftA2 VectorRef parseExpression integer
parseCons      = liftA2 Cons      parseExpression parseExpression
parseVector    = liftA  Vector    (many parseExpression)
parseValues    = liftA  Values    (many parseExpression)

parseSpecialForm, parseApplication :: Parser (Expr Name)
parseSpecialForm = caseParser [ ("if"        , parseIf       )
                              , ("let"       , parseLet      )
                              , ("let-values", parseLetValues)
                              , ("cons"      , parseCons     )
                              , ("vector"    , parseVector   )
                              , ("values"    , parseValues   )
                              , ("car"       , parseCar      )
                              , ("cdr"       , parseCdr      )
                              , ("vector-ref", parseVectorRef)
                              ]
parseApplication = liftA2 ProcCall proc args
    where
      proc = identifier
      args = many parseExpression

parseExpression :: Parser (Expr Name)
parseExpression = parseAtom <|> parseList
    where
      parseAtom = parseVariable <|> parseConstant
      parseList = parens $ try parseSpecialForm
                           <|> parseApplication

parseProgram :: Parser (Prog Name)
parseProgram = try (liftA2 Prog (pure []) parseExpression)
               <|> (tagged "begin" $ parseBegin)
    where
      parseBegin = try (do expr <- parseExpression
                           return $ Prog [] expr)
                   <|> (do defn <- parseDefine
                           Prog defns expr <- parseBegin
                           return $ Prog (defn:defns) expr)

parse :: String -> (Prog Name)
parse = (either (\_ -> error "parse error") id)
      . runParser parseProgram () ""
      . scan

example = "(begin (define (loop-on-pair-19 the-closure-160 the-closure-161 the-formals-162 the-formals-163) (argument-types real real real real real) (if (< (abs (- the-formals-162 the-formals-163)) 0.00001) the-formals-163 (loop-on-pair-19 the-closure-161 the-closure-161 the-formals-163 (/ (+ the-formals-163 (/ the-closure-160 the-formals-163)) 2)))) (let ((the-formals-64 (real 2))) (let ((anf-132 (real 1.0))) (cons 1.4142135623730951 (loop-on-pair-19 the-formals-64 the-formals-64 anf-132 (/ (+ anf-132 (/ the-formals-64 anf-132)) 2))))))"

bigExample = "(begin (define (operation-2 the-closure the-formals) (argument-types (vector) (vector) (cons real real)) (let () (operation-3 (vector) (vector)))) (define (recur-on-closure-4 the-closure the-formals) (argument-types (vector (vector (vector real))) (vector (vector (vector real))) (vector (vector real) (vector (vector (vector (vector real)))))) (let ((recur the-formals)) (kernel-on-closure-7 (vector-ref the-closure 0) (vector recur)))) (define (numeric-fix-on-pair-9 the-closure the-formals) (argument-types (vector) (cons (vector real) (cons real (vector))) real) (let ((f (car the-formals)) (start (car (cdr the-formals)))) (operation-11 (vector f start) (vector)))) (define (close-enuf?-on-pair-13 the-closure the-formals) (argument-types (vector) (cons real real) bool) (let ((a (car the-formals)) (b (cdr the-formals))) (let ((temp-15 (cons (abs (let ((temp-14 (cons a b))) (- (car temp-14) (cdr temp-14)))) 0.00001))) (< (car temp-15) 0.00001)))) (define (operation-16 the-closure the-formals) (argument-types (vector (vector real) (vector (vector real) (vector (vector (vector (vector real))))) real) () real) (let () (loop-on-pair-19 (vector-ref the-closure 1) (cons (vector-ref the-closure 2) (f-on-real-18 (vector-ref the-closure 0) (vector-ref the-closure 2)))))) (define (operation-20 the-closure the-formals) (argument-types (vector (vector real) (vector (vector (vector (vector real)))) real) () real) (let () (loop-on-pair-22 (vector-ref the-closure 1) (cons (vector-ref the-closure 2) (f-on-real-18 (vector-ref the-closure 0) (vector-ref the-closure 2)))))) (define (f-on-real-18 the-closure the-formals) (argument-types (vector real) real real) (let ((guess the-formals)) (let ((temp-25 (cons (let ((temp-24 (cons guess (let ((temp-23 (cons (vector-ref the-closure 0) guess))) (/ (car temp-23) (cdr temp-23)))))) (+ (car temp-24) (cdr temp-24))) 2))) (/ (car temp-25) 2)))) (define (operation-26 the-closure the-formals) (argument-types (vector) (vector) (cons real real)) (let () (operation-27 (vector) ()))) (define (kernel-on-closure-7 the-closure the-formals) (argument-types (vector (vector real)) (vector (vector (vector (vector real)))) (vector (vector real) (vector (vector (vector (vector real)))))) (let ((loop the-formals)) (vector (vector-ref the-closure 0) loop))) (define (loop-on-pair-22 the-closure the-formals) (argument-types (vector (vector (vector (vector real)))) (cons real real) real) (let ((y the-formals)) (loop-on-pair-19 (recur-on-closure-4 (vector-ref the-closure 0) (vector-ref the-closure 0)) y))) (define (operation-27 the-closure the-formals) (argument-types (vector) () (cons real real)) (let () (cons 1.4142135623730951 (square-root-on-real-29 (vector) (real 2))))) (define (the-z-combinator-on-closure-30 the-closure the-formals) (argument-types (vector) (vector (vector real)) (vector (vector real) (vector (vector (vector (vector real)))))) (let ((kernel the-formals)) (recur-on-closure-4 (vector kernel) (vector kernel)))) (define (operation-11 the-closure the-formals) (argument-types (vector (vector real) real) (vector) real) (let () (operation-32 (vector (vector-ref the-closure 0) (vector-ref the-closure 1)) (the-z-combinator-on-closure-30 (vector) (vector (vector-ref the-closure 0)))))) (define (heron-step-on-real-33 the-closure the-formals) (argument-types (vector) real (vector real)) (let ((x the-formals)) (vector x))) (define (operation-32 the-closure the-formals) (argument-types (vector (vector real) real) (vector (vector real) (vector (vector (vector (vector real))))) real) (let ((loop the-formals)) (operation-16 (vector (vector-ref the-closure 0) loop (vector-ref the-closure 1)) ()))) (define (operation-1 the-closure the-formals) (argument-types (vector) () (cons real real)) (let () (operation-34 (vector) (vector)))) (define (operation-34 the-closure the-formals) (argument-types (vector) (vector) (cons real real)) (let () (operation-2 (vector) (vector)))) (define (operation-35 the-closure the-formals) (argument-types (vector real) () real) (let () (vector-ref the-closure 0))) (define (operation-3 the-closure the-formals) (argument-types (vector) (vector) (cons real real)) (let () (operation-26 (vector) (vector)))) (define (square-root-on-real-29 the-closure the-formals) (argument-types (vector) real real) (let ((x the-formals)) (numeric-fix-on-pair-9 (vector) (cons (heron-step-on-real-33 (vector) x) (cons (real 1.0) (vector)))))) (define (loop-on-pair-19 the-closure the-formals) (argument-types (vector (vector real) (vector (vector (vector (vector real))))) (cons real real) real) (let ((old (car the-formals)) (new (cdr the-formals))) (if (close-enuf?-on-pair-13 (vector) (cons old new)) (operation-35 (vector new) ()) (operation-20 (vector (vector-ref the-closure 0) (vector-ref the-closure 1) new) ())))) (operation-1 (vector) ()))"