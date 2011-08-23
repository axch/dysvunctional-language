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

type NormalT = WriterT [LetBinding]

normalize :: Expr -> NormalT Unique Expr
normalize e
    | isSimpleExpr e
    = return e
    | otherwise
    = do name <- lift $ uniqueName "anf"
         e'   <- lift $ anfExpr e
         tell [(name, e')]
         return (Var name)

evalNormalT :: Monad m => NormalT m Expr -> m Expr
evalNormalT a = do (body, bindings) <- runWriterT a
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
    = evalNormalT $ liftA  Car       (normalize e)
anfExpr (Cdr e)
    = evalNormalT $ liftA  Cdr       (normalize e)
anfExpr (VectorRef e i)
    = evalNormalT $ liftA2 VectorRef (normalize e) (pure i)
anfExpr (Cons e1 e2)
    = evalNormalT $ liftA2 Cons      (normalize e1) (normalize e2)
anfExpr (Vector es)
    = evalNormalT $ liftA  Vector    (mapM normalize es)
anfExpr (Values es)
    = evalNormalT $ liftA  Values    (mapM normalize es)
anfExpr (ProcCall proc args)
    = evalNormalT $ liftA2 ProcCall  (pure proc) (mapM normalize args)

anfDefn :: Defn -> Unique Defn
anfDefn (Defn proc args body) = liftA (Defn proc args) (anfExpr body)

anfProg :: Prog -> Unique Prog
anfProg (Prog defns expr) = liftA2 Prog (mapM anfDefn defns) (anfExpr expr)

anf :: Prog -> Unique Prog
anf = anfProg