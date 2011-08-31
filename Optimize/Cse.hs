{-# LANGUAGE NoImplicitPrelude, TypeOperators, MultiParamTypeClasses #-}
module FOL.Optimize.Cse where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Data.List
import Data.Maybe

import Debug.Trace

data CanExpr
    = CanVar Name
    | CanNil
    | CanBool Bool
    | CanReal Real
      deriving (Eq, Show)

instance CanExpr :<: Expr where
    inj (CanVar x)  = Var x
    inj CanNil      = Nil
    inj (CanBool b) = Bool b
    inj (CanReal r) = Real r

maybeCanExpr :: Expr -> Maybe CanExpr
maybeCanExpr (Var x)  = Just (CanVar x)
maybeCanExpr Nil      = Just CanNil
maybeCanExpr (Bool b) = Just (CanBool b)
maybeCanExpr (Real r) = Just (CanReal r)
maybeCanExpr _        = Nothing

isConst :: Expr -> Bool
isConst Nil      = True
isConst (Bool _) = True
isConst (Real _) = True
isConst _        = False

cseExpr :: [(Expr, CanExpr)] -> Expr -> Expr
cseExpr env expr
    | Just can_expr <- lookup expr env
    = inj can_expr
    | isConst expr
    = expr
cseExpr env (Var x)
    = error $ "Unbound variable: " ++ pprint x
cseExpr env (If p c a)
    = If (cseExpr env p) (cseExpr env c) (cseExpr env a)
cseExpr env (Let bindings body)
    = mkLet bs'' body'
    where
      Bindings bs' = fmap (cseExpr env) bindings
      bs''  = [(x, e')
                   | (x, e') <- bs', isNothing (maybeCanExpr e')]
      env'  = [(e',    CanVar x) | (x, e') <- bs'']
           ++ [(Var x, CanVar x) | (x, _ ) <- bs'']
           ++ [(Var x, ce)
                   | (x, e') <- bs', ce <- maybeToList $ maybeCanExpr e']
      body' = cseExpr (env' ++ env) body
cseExpr env (LetValues bindings body)
    = mkLetValues bs' body'
    where
      Bindings bs' = fmap (cseExpr env) bindings
      (bs1', bs2') = partition is_values bs'
      is_values (_, Values _) = True
      is_values _             = False
      env'  = [alias x e | (xs, Values es) <- bs1', (x, e) <- zip xs es]
           ++ [(Var x, CanVar x) | (xs, _) <- bs2', x <- xs]
      body' = cseExpr (env' ++ env) body
      alias x e
          | Just ce <- maybeCanExpr e
          = (Var x, ce)
          | otherwise
          = (e, CanVar x)

cseExpr env (Car e)              = Car (cseExpr env e)
cseExpr env (Cdr e)              = Cdr (cseExpr env e)
cseExpr env (VectorRef e i)      = VectorRef (cseExpr env e) i
cseExpr env (Cons e1 e2)         = Cons (cseExpr env e1) (cseExpr env e2)
cseExpr env (Vector es)          = Vector (map (cseExpr env) es)
cseExpr env (Values es)          = Values (map (cseExpr env) es)
cseExpr env (ProcCall proc args) = ProcCall proc (map (cseExpr env) args)

cseDefn :: [(Expr, CanExpr)] -> Defn -> Defn
cseDefn env (Defn proc args body) = Defn proc args body'
    where
      body' = cseExpr (env' ++ env) body
      env'  = [(Var x, CanVar x) | (x, _) <- args]

cseProg :: [(Expr, CanExpr)] -> Prog -> Prog
cseProg env (Prog defns expr) = Prog (map (cseDefn env) defns) (cseExpr env expr)

cse :: Prog -> Prog
cse = cseProg []
