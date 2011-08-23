{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Anf where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Unique

import Control.Applicative
import Control.Monad.Writer

isSimpleExpr :: Expr -> Bool
isSimpleExpr (Var _)  = True
isSimpleExpr Nil      = True
isSimpleExpr (Bool _) = True
isSimpleExpr (Real _) = True
isSimpleExpr _        = False

type SimpleT = WriterT [LetBinding]

simplify :: Expr -> SimpleT Unique Expr
simplify e
    | isSimpleExpr e
    = return e
    | otherwise
    = do name <- lift $ uniqueName "anf"
         e'   <- lift $ anfExpr e
         tell [(name, e')]
         return (Var name)

evalSimpleT :: Monad m => SimpleT m Expr -> m Expr
evalSimpleT a = do (body, bindings) <- runWriterT a
                   return $ mkLet bindings body

mkLet :: [LetBinding] -> Expr -> Expr
mkLet []       body =              body
mkLet bindings body = Let bindings body

anfExpr :: Expr -> Unique Expr
anfExpr e
    | isSimpleExpr e
    = return e
anfExpr (If p c a)
    = liftA3 If (anfExpr p) (anfExpr c) (anfExpr a)
anfExpr (Let bindings body)
    = do bindings' <- sequence [liftA2 (,) (pure x) (anfExpr e) | (x, e) <- bindings]
         body'     <- anfExpr body
         return (Let bindings' body')
anfExpr (LetValues bindings body)
    = do bindings' <- sequence [liftA2 (,) (pure xs) (anfExpr e) | (xs, e) <- bindings]
         body'     <- anfExpr body
         return (LetValues bindings' body')
anfExpr (Car e)
    = evalSimpleT $ liftA  Car       (simplify e)
anfExpr (Cdr e)
    = evalSimpleT $ liftA  Cdr       (simplify e)
anfExpr (VectorRef e i)
    = evalSimpleT $ liftA2 VectorRef (simplify e) (pure i)
anfExpr (Cons e1 e2)
    = evalSimpleT $ liftA2 Cons      (simplify e1) (simplify e2)
anfExpr (Vector es)
    = evalSimpleT $ liftA  Vector    (mapM simplify es)
anfExpr (Values es)
    = evalSimpleT $ liftA  Values    (mapM simplify es)
anfExpr (ProcCall proc args)
    = evalSimpleT $ liftA2 ProcCall  (pure proc) (mapM simplify args)

anfDefn :: Defn -> Unique Defn
anfDefn (Defn proc args body) = liftA (Defn proc args) (anfExpr body)

anfProg :: Prog -> Unique Prog
anfProg (Prog defns expr) = liftA2 Prog (mapM anfDefn defns) (anfExpr expr)
