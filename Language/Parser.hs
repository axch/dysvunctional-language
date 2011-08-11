module FOL.Language.Parser where

import FOL.Language.Expression
import FOL.Language.Token

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

-- Accept a token 't' with the result 'd' when 't' is the double 'd'.
double :: Parser Double
double = tok maybeDouble
    where
      maybeDouble (TokDouble d) = Just d
      maybeDouble _             = Nothing

-- To parse 'vector-ref's we must be able to parse integers, but I am
-- lazy to modify the tokenizer.  Whenever an integer is expected, it
-- is parsed as a double and is truncated.
integer :: Parser Integer
integer = truncate <$> double

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

parseTyNil, parseTyCons, parseTyVector, parseTyValues, parseType :: Parser Type
parseTyNil    = TyNil <$ parseEmptyList
parseTyCons   = liftA2 TyCons parseType parseType
parseTyVector = liftA  TyVector (many parseType)
parseTyValues = liftA  TyValues (many parseType)

parseType = parseTyNil <|> parseAtomicType <|> parens parseCompoundType
    where
      parseAtomicType   = caseParser [ ("real",   return TyReal)
                                     , ("bool",   return TyBool)
                                     ]
      parseCompoundType = caseParser [ ("cons",   parseTyCons  )
                                     , ("vector", parseTyVector)
                                     , ("values", parseTyValues)
                                     ]

-- Parsing programs

parseNil :: Parser Expr
parseNil = Nil <$ parseEmptyList

parseVariable :: Parser Expr
parseVariable = Var <$> identifier

parseConstant :: Parser Expr
parseConstant = try parseNil <|> tok maybeConstant
    where
      maybeConstant (TokBool b)   = Just (Boolean b)
      maybeConstant (TokDouble d) = Just (Number d)
      maybeConstant _             = Nothing

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

parseDefine :: Parser Defn
parseDefine = tagged "define" $ do
                (proc, args) <- parens $ liftA2 (,) identifier (many identifier)
                types <- tagged "argument-types" (many parseType)
                let proc_type = last types
                    arg_types = init types
                body <- parseExpression
                return $ Defn (proc, proc_type) (zip args arg_types) body

parseIf, parseLet, parseLetValues, parseCar, parseCdr :: Parser Expr
parseVectorRef, parseCons, parseVector, parseValues   :: Parser Expr

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

parseSpecialForm, parseApplication :: Parser Expr
parseSpecialForm = caseParser [ ("cons"      , parseCons     )
                              , ("if"        , parseIf       )
                              , ("let"       , parseLet      )
                              , ("let-values", parseLetValues)
                              ]
parseApplication = liftA2 ProcCall proc args
    where
      proc = identifier
      args = many parseExpression

parseExpression :: Parser Expr
parseExpression = parseAtom <|> parseList
    where
      parseAtom = parseVariable <|> parseConstant
      parseList = parens $ try parseSpecialForm
                           <|> parseApplication

parseProgram :: Parser Prog
parseProgram = try (liftA2 Prog (pure []) parseExpression)
               <|> (tagged "begin" $ parseBegin)
    where
      parseBegin = try (do expr <- parseExpression
                           return $ Prog [] expr)
                   <|> (do defn <- parseDefine
                           Prog defns expr <- parseBegin
                           return $ Prog (defn:defns) expr)

parse :: String -> Prog
parse = (either (\_ -> error "parse error") id)
      . runParser parseProgram () ""
      . scan

example = "(begin (define (loop-on-pair-19 the-closure-160 the-closure-161 the-formals-162 the-formals-163) (argument-types real real real real real) (if (< (abs (- the-formals-162 the-formals-163)) 0.00001) the-formals-163 (loop-on-pair-19 the-closure-161 the-closure-161 the-formals-163 (/ (+ the-formals-163 (/ the-closure-160 the-formals-163)) 2)))) (let ((the-formals-64 (real 2))) (let ((anf-132 (real 1.0))) (cons 1.4142135623730951 (loop-on-pair-19 the-formals-64 the-formals-64 anf-132 (/ (+ anf-132 (/ the-formals-64 anf-132)) 2))))))"