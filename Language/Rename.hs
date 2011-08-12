{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Rename where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Unique

import Control.Monad.State
import Control.Applicative

import Data.List
import Data.Maybe
import Data.Traversable (sequenceA)

type Rename = State NameList
type NameList = [Name]

evalRename :: Rename a -> a
evalRename = flip evalState []

maybeRename :: NameList -> Name -> Unique Name
maybeRename names name
    | name `elem` names = uniqueName (name2str name)
    | otherwise         = return name

renameExpr :: [(Name, Unique Name)] -> Expr Name -> Rename (Expr (Unique Name))
renameExpr env (Var x) = return (Var x')
    where
      x' = fromMaybe (return x) (lookup x env)
renameExpr _ Nil = return Nil
renameExpr _ (Bool b) = return (Bool b)
renameExpr _ (Real r) = return (Real r)
renameExpr env (If p c a) = liftA3 If (renameExpr env p) (renameExpr env c) (renameExpr env a)
renameExpr env (Let bindings body)
    = do names <- get
         let (xs, es) = unzip bindings
             xs' = map (maybeRename names) xs
         put (xs `union` names)
         body' <- renameExpr (zip xs xs' ++ env) body
         es' <- mapM (renameExpr env) es
         let bindings' = zip xs' es'
         return (Let bindings' body')
renameExpr env (LetValues bindings body)
    = do names <- get
         let (xs, es) = unzip bindings
             xs' = map (map (maybeRename names)) xs
         put (concat xs `union` names)
         body' <- renameExpr (zip (concat xs) (concat xs') ++ env) body
         es' <- mapM (renameExpr env) es
         let bindings' = zip xs' es'
         return (LetValues bindings' body')
renameExpr env (Car e) = Car <$> renameExpr env e
renameExpr env (Cdr e) = Cdr <$> renameExpr env e
renameExpr env (VectorRef e i) = liftA2 VectorRef (renameExpr env e) (pure i)
renameExpr env (Cons e1 e2) = liftA2 Cons (renameExpr env e1) (renameExpr env e2)
renameExpr env (Vector es) = Vector <$> mapM (renameExpr env) es
renameExpr env (Values es) = Values <$> mapM (renameExpr env) es
renameExpr env (ProcCall proc args) = liftA2 ProcCall (pure (return proc)) args'
    where
      args' = mapM (renameExpr env) args

renameDefn :: [(Name, Unique Name)] -> Defn Name -> Rename (Defn (Unique Name))
renameDefn env (Defn proc args body)
    = do names <- get
         let (proc_name, proc_type) = proc
             (arg_names, arg_types) = unzip args
             arg_names' = map (maybeRename names) arg_names
             proc' = (return proc_name, proc_type)
             args' = zip arg_names' arg_types
         put ((proc_name : arg_names) `union` names)
         body' <- renameExpr (zip arg_names arg_names' ++ env) body
         return (Defn proc' args' body')

renameProg :: [(Name, Unique Name)] -> Prog Name -> Rename (Prog (Unique Name))
renameProg env (Prog defns expr)
    = liftA2 Prog (mapM (renameDefn env) defns) (renameExpr env expr)

rename :: Prog Name -> Unique (Prog Name)
rename = sequenceA . evalRename . renameProg []