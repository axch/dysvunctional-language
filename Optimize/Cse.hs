{-# LANGUAGE NoImplicitPrelude, TypeOperators, MultiParamTypeClasses #-}
module FOL.Optimize.Cse where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Data.Maybe

data CanName = CanVar Name
             | CanNil
             | CanBool Bool
             | CanReal Real

instance CanName :<: Expr where
    inj (CanVar x)  = Var x
    inj CanNil      = Nil
    inj (CanBool b) = Bool b
    inj (CanReal r) = Real r

data SymExpr = SymVar Name
             | SymNil
             | SymBool Bool
             | SymReal Real
             | SymIf SymExpr SymExpr SymExpr
             | SymCar SymExpr
             | SymCdr SymExpr
             | SymVectorRef SymExpr Int
             | SymValuesRef SymExpr Int
             | SymCons SymExpr SymExpr
             | SymVector [SymExpr]
             | SymValues [SymExpr]
             | SymProcCall Name [SymExpr]
               deriving Eq

instance CanName :<: SymExpr where
    inj (CanVar x)  = SymVar x
    inj CanNil      = SymNil
    inj (CanBool b) = SymBool b
    inj (CanReal r) = SymReal r

isVariable, isConstant :: SymExpr -> Bool
isVariable (SymVar _)  = True
isVariable _           = False

isConstant SymNil      = True
isConstant (SymBool _) = True
isConstant (SymReal _) = True
isConstant _           = False

type CseEnv = [(SymExpr, CanName)]

augmentEnv :: [SymExpr] -> [CanName] -> CseEnv -> (CseEnv, [Bool])
augmentEnv ss ns env = (env' ++ env, map (isAcceptableAlias env) ss)
    where
      env' = concat (zipWith bind ss ns)
      bind s n
          | Just c <- lookup s env
          = [(inj n, c)]
      bind (SymVar _) n
          = [(inj n, n)]
      -- bind (SymVar x)  n
      --     = [(inj n, CanVar x)]
      bind SymNil n
          = [(inj n, CanNil)]
      bind (SymBool b) n
          = [(inj n, CanBool b)]
      bind (SymReal r) n
          = [(inj n, CanReal r)]
      bind s n
          = [(inj n, n), (s, n)]

isAcceptableAlias :: CseEnv -> SymExpr -> Bool
isAcceptableAlias env s = isConstant s || (isVariable s && isInScope env s)

isInScope :: CseEnv -> SymExpr -> Bool
isInScope env s = isConstant s || isJust (lookup s env)

cseExpr :: CseEnv -> Expr -> (SymExpr, Expr)
cseExpr env (Var x)
    | Just n <- lookup (SymVar x) env
    = (inj n, inj n)
    | otherwise
    = error $ "Unbound variable: " ++ pprint x
cseExpr _ Nil = (SymNil, Nil)
cseExpr _ e@(Bool b) = (SymBool b, e)
cseExpr _ e@(Real r) = (SymReal r, e)
cseExpr env (If p c a)
    = (SymIf sp sc sa, If p' c' a')
    where
      (sp, p') = cseExpr env p
      (sc, c') = cseExpr env c
      (sa, a') = cseExpr env a
cseExpr env (Let (Bindings bs) body)
    = (sbody, mkLet bs' body')
    where
      (xs, es) = unzip bs
      (ss, es') = unzip $ map (cseExpr env) es
      (env', dead) = augmentEnv ss (map CanVar xs) env
      (sbody, body') = cseExpr env' body
      bs' = [(x, e') | (x, False, e') <- zip3 xs dead es']
cseExpr env (LetValues (Bindings bs) body)
    = (sbody, mkLetValues bs' body')
    where
      (xss, es) = unzip bs
      (ss, es') = unzip $ map (cseExpr env) es
      sss = zipWith destructure xss ss
      destructure _ (SymValues syms) = syms
      destructure xs s = [SymValuesRef s i | i <- [1..length xs]]
      (env', dead) = augmentEnv (concat sss) (map CanVar (concat xss)) env
      (sbody, body') = cseExpr env' body
      bs'| any not dead = zip xss es'
         | otherwise    = []
cseExpr env (Car e)              = (SymCar se, Car e')
    where
      (se, e') = cseExpr env e
cseExpr env (Cdr e)              = (SymCdr se, Cdr e')
    where
      (se, e') = cseExpr env e
cseExpr env (VectorRef e i)      = (SymVectorRef se i, VectorRef e' i)
    where
      (se, e') = cseExpr env e
cseExpr env (Cons e1 e2)         = (SymCons se1 se2, Cons e1' e2')
    where
      (se1, e1') = cseExpr env e1
      (se2, e2') = cseExpr env e2
cseExpr env (Vector es)          = (SymVector ses, Vector es')
    where
      (ses, es') = unzip (map (cseExpr env) es)
cseExpr env (Values es)          = (SymValues ses, Values es')
    where
      (ses, es') = unzip (map (cseExpr env) es)
cseExpr env (ProcCall proc args) = (SymProcCall proc sargs, ProcCall proc args')
    where
      (sargs, args') = unzip (map (cseExpr env) args)

cseDefn :: CseEnv -> Defn -> Defn
cseDefn env (Defn proc args body) = Defn proc args body'
    where
      (_, body') = cseExpr (env' ++ env) body
      env'  = [(SymVar x, CanVar x) | (x, _) <- args]

cseProg :: CseEnv -> Prog -> Prog
cseProg env (Prog defns expr) = Prog (map (cseDefn env) defns) expr'
    where
      (_, expr') = cseExpr env expr

cse :: Prog -> Prog
cse = cseProg []
