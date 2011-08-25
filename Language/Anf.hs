{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Anf where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Unique

import qualified Data.Traversable as Traversable

import Control.Applicative
import Control.Monad.Writer

isSimpleExpr :: Expr -> Bool
isSimpleExpr (Var _)  = True
isSimpleExpr Nil      = True
isSimpleExpr (Bool _) = True
isSimpleExpr (Real _) = True
isSimpleExpr _        = False

type NormalT = WriterT [(Name, Expr)]

normalize :: Expr -> NormalT Unique Expr
normalize e
    | isSimpleExpr e
    = return e
    | otherwise
    = do name <- lift $ uniqueName "anf"
         e'   <- lift $ anfExpr e
         tell [(name, e')]
         return (Var name)

toLet :: Monad m => NormalT m Expr -> m Expr
toLet a = do (body, bindings) <- runWriterT a
             return $ mkLet bindings body

anfExpr :: Expr -> Unique Expr
anfExpr e
    | isSimpleExpr e
    = return e

anfExpr (If p c a)
    = liftA3 If        (anfExpr p) (anfExpr c) (anfExpr a)
anfExpr (Let bindings body)
    = liftA2 Let       (Traversable.mapM anfExpr bindings) (anfExpr body)
anfExpr (LetValues bindings body)
    = liftA2 LetValues (Traversable.mapM anfExpr bindings) (anfExpr body)

anfExpr (Car e)
    = toLet $ liftA  Car       (normalize e)
anfExpr (Cdr e)
    = toLet $ liftA  Cdr       (normalize e)
anfExpr (VectorRef e i)
    = toLet $ liftA2 VectorRef (normalize e) (pure i)
anfExpr (Cons e1 e2)
    = toLet $ liftA2 Cons      (normalize e1) (normalize e2)
anfExpr (Vector es)
    = toLet $ liftA  Vector    (mapM normalize es)
anfExpr (Values es)
    = toLet $ liftA  Values    (mapM normalize es)
anfExpr (ProcCall proc args)
    = toLet $ liftA2 ProcCall  (pure proc) (mapM normalize args)

anfDefn :: Defn -> Unique Defn
anfDefn (Defn proc args body) = liftA (Defn proc args) (anfExpr body)

anfProg :: Prog -> Unique Prog
anfProg (Prog defns expr) = liftA2 Prog (mapM anfDefn defns) (anfExpr expr)

anf :: Prog -> Unique Prog
anf = anfProg