{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.AlphaRn (alphaRn) where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Unique

import Control.Monad.State
import Control.Applicative

import Data.List
import Data.Maybe

type AlphaRnT = StateT NameList
type NameList = [Name]

evalAlphaRnT :: Monad m => AlphaRnT m a -> m a
evalAlphaRnT = flip evalStateT []

rename :: NameList -> Name -> Unique Name
rename ns n@(Name name)
    | n `elem` ns = uniqueName name
    | otherwise   = return n

alphaRnExpr :: [(Name, Name)] -> Expr -> AlphaRnT Unique Expr
alphaRnExpr env (Var x) = return (Var x')
    where
      x' = fromMaybe x (lookup x env)
alphaRnExpr _ Nil       = return Nil
alphaRnExpr _ (Bool b)  = return (Bool b)
alphaRnExpr _ (Real r)  = return (Real r)
alphaRnExpr env (If p c a)
    = liftA3 If (alphaRnExpr env p)
                (alphaRnExpr env c)
                (alphaRnExpr env a)
alphaRnExpr env (Let bindings body)
    = do used_names <- get
         xs' <- lift $ mapM (rename used_names) xs
         put (xs `union` used_names)
         body' <- alphaRnExpr (zip xs xs' ++ env) body
         es' <- mapM (alphaRnExpr env) es
         let bindings' = zip xs' es'
         return (Let bindings' body')
    where
      (xs, es) = unzip bindings
alphaRnExpr env (LetValues bindings body)
    = do used_names <- get
         xs' <- lift $ mapM (mapM (rename used_names)) xs
         put (concat xs `union` used_names)
         body' <- alphaRnExpr (zip (concat xs) (concat xs') ++ env) body
         es' <- mapM (alphaRnExpr env) es
         let bindings' = zip xs' es'
         return (LetValues bindings' body')
    where
      (xs, es) = unzip bindings
alphaRnExpr env (Car e) = Car <$> alphaRnExpr env e
alphaRnExpr env (Cdr e) = Cdr <$> alphaRnExpr env e
alphaRnExpr env (VectorRef e i)
    = liftA2 VectorRef (alphaRnExpr env e) (pure i)
alphaRnExpr env (Cons e1 e2)
    = liftA2 Cons (alphaRnExpr env e1) (alphaRnExpr env e2)
alphaRnExpr env (Vector es) = Vector <$> mapM (alphaRnExpr env) es
alphaRnExpr env (Values es) = Values <$> mapM (alphaRnExpr env) es
alphaRnExpr env (ProcCall proc args)
    = liftA2 ProcCall (pure proc) (mapM (alphaRnExpr env) args)

alphaRnDefn :: [(Name, Name)] -> Defn -> AlphaRnT Unique Defn
alphaRnDefn env (Defn proc args body)
    = do used_names <- get
         arg_names' <- lift $ mapM (rename used_names) arg_names
         let args' = zip arg_names' arg_types
         put ((proc_name : arg_names) `union` used_names)
         body' <- alphaRnExpr (zip arg_names arg_names' ++ env) body
         return (Defn proc args' body')
    where
      (proc_name, proc_type) = proc
      (arg_names, arg_types) = unzip args

alphaRnProg :: [(Name, Name)] -> Prog -> AlphaRnT Unique Prog
alphaRnProg env (Prog defns expr)
    = liftA2 Prog (mapM (alphaRnDefn env) defns) (alphaRnExpr env expr)

alphaRn :: Prog -> Unique Prog
alphaRn = evalAlphaRnT . alphaRnProg []
