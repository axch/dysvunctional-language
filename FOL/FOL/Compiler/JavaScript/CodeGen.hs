{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Compiler.JavaScript.CodeGen where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.TypeCheck
import FOL.Language.Parser
import FOL.Language.Pretty
import FOL.Compiler.JavaScript.Syntax

compileProg :: Prog -> JsProg
compileProg (Prog defns expr)
    = JsProg $ prelude ++ map compileDefn defns ++ [entry_point]
    where
      entry_point = JsDefn (Name "__main__") [] (compileExpr expr)

compileDefn :: Defn -> JsDefn
compileDefn (Defn shaped_name shaped_args body)
    = JsDefn name args (compileExpr body)
    where
      name = fst shaped_name
      args = map fst shaped_args

compileExpr :: Expr -> JsExpr
compileExpr (Var x) = JsVar x
compileExpr Nil = JsNull
compileExpr (Bool b) = JsBool b
compileExpr (Real r) = JsReal r
compileExpr (If p c a)
    = JsIf (compileExpr p) (compileExpr c) (compileExpr a)
compileExpr (Let (Bindings bindings) body)
    = JsFunctionCall func (map compileExpr es)
    where
      (xs, es) = unzip bindings
      func = JsFunction xs (compileExpr body)
compileExpr (LetValues (Bindings bindings) body)
    = JsFunctionCall func [compileExpr e]
    where
      [(xs, e)] = bindings
      -- This is not hygenic, but who cares...
      func = JsFunction [Name "__arg__"]
           $ JsFunctionCall (JsFunction xs (compileExpr body))
                            [JsAccess (JsVar (Name "__arg__")) i
                                 | i <- [0..length xs-1]]
compileExpr (Car e) = JsAccess (compileExpr e) 0
compileExpr (Cdr e) = JsAccess (compileExpr e) 1
compileExpr (VectorRef e i) = JsAccess (compileExpr e) i
compileExpr (Cons e1 e2) = JsArray [compileExpr e1, compileExpr e2]
compileExpr (Vector es) = JsArray (map compileExpr es)
compileExpr (Values es) = JsArray (map compileExpr es)
compileExpr (Lambda x e) = JsFunction [x] (compileExpr e)
compileExpr (ProcCall proc args)
    | Just op  <- lookup proc ops
    , [e1, e2] <- args
    = JsInfixOpApplication op (compileExpr e1) (compileExpr e2)
    | otherwise
    = JsFunctionCall (JsVar proc) (map compileExpr args)

ops :: [(Name, Op)]
ops = [ (Name "+", Add)
      , (Name "-", Sub)
      , (Name "*", Mul)
      , (Name "/", Div)
      , (Name "=",  Eq)
      , (Name "/=", Ne)
      , (Name ">=", Ge)
      , (Name "<=", Le)
      , (Name ">",  Gt)
      , (Name "<",  Lt)
      ]

prelude = [ JsDefn (Name "real") [Name "x"]
            (JsVar (Name "x"))
          , JsDefn (Name "isZero") [Name "x"]
            (JsInfixOpApplication Eq (JsVar (Name "x")) (JsReal 0.0))
          , JsDefn (Name "isPositive") [Name "x"]
            (JsInfixOpApplication Gt (JsVar (Name "x")) (JsReal 0.0))
          , JsDefn (Name "isNegative") [Name "x"]
            (JsInfixOpApplication Lt (JsVar (Name "x")) (JsReal 0.0))
          ]

program = parse "(begin (define (operation-19 the-closure-141 the-closure-142 the-formals-143 the-formals-144) (argument-types real real real real real) (if (< (abs (- the-formals-143 the-formals-144)) .00001) the-formals-144 (operation-19 the-closure-142 the-closure-142 the-formals-144 (/ (+ the-formals-144 (/ the-closure-141 the-formals-144)) 2)))) (let ((the-formals-92 (real 2))) (let ((anf-79 (real 1.))) (cons 1.4142135623730951 (operation-19 the-formals-92 the-formals-92 anf-79 (/ (+ anf-79 (/ the-formals-92 anf-79)) 2))))))"

program2 = parse "(begin (define (operation-19 the-formals-95) (argument-types real bool) (if (= the-formals-95 0) #t (let ((the-formals-99 (- the-formals-95 1))) (if (= the-formals-99 0) #f (operation-19 (- the-formals-99 1)))))) (operation-19 (real 5)))"

program3 = parse "(begin (define (operation-363 the-formals-7993 the-formals-7994 the-formals-7995) (argument-types real real real (values real real)) (if (= the-formals-7995 0) (values the-formals-7993 the-formals-7994) (let-values (((the-formals-8282 the-formals-8283) (operation-423 (real .001) the-formals-7994))) (operation-363 (operation-576 the-formals-7993 the-formals-8282) (operation-576 the-formals-7994 the-formals-8283) (+ the-formals-7995 -1))))) (define (operation-423 the-formals-9125 the-formals-9127) (argument-types real real (values real real)) (values the-formals-9125 (* the-formals-9125 the-formals-9127))) (define (operation-576 the-formals-9141 the-formals-9142) (argument-types real real real) (+ the-formals-9141 the-formals-9142)) (let-values (((receipt-7721 receipt-7722) (operation-363 (real 0) (real 1) (real 1000)))) (cons receipt-7721 receipt-7722)))"
