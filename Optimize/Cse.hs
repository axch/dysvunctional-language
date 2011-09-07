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

maybeCanName :: Expr -> Maybe CanName
maybeCanName (Var x)  = Just (CanVar x)
maybeCanName Nil      = Just CanNil
maybeCanName (Bool b) = Just (CanBool b)
maybeCanName (Real r) = Just (CanReal r)
maybeCanName _        = Nothing

isVariable, isConstant, isValues, isNotValues :: Expr -> Bool
isVariable (Var _)  = True
isVariable _        = False

isConstant Nil      = True
isConstant (Bool _) = True
isConstant (Real _) = True
isConstant _        = False

isValues (Values _) = True
isValues _          = False

isNotValues = not . isValues

isCanName, isNotCanName :: Expr -> Bool
isCanName    = isJust    . maybeCanName
isNotCanName = isNothing . maybeCanName

cseExpr :: [(Expr, CanName)] -> Expr -> Expr
cseExpr env e
    | Just a <- lookup e env
    = inj a
    | isConstant e, let Just a = maybeCanName e
    = inj a
cseExpr _ (Var x)
    = error $ "Unbound variable: " ++ pprint x
cseExpr env (If p c a)
    = If (cseExpr env p) (cseExpr env c) (cseExpr env a)
cseExpr env (Let (Bindings bs) body)
    = mkLet bs1' body'
    where
      -- CSE each RHS
      bs'   = [(x, cseExpr env e) | (x, e) <- bs]
      -- Flush bindings with atomic RHSs
      bs1'  = [b' | b'@(_, e') <- bs', isNotCanName e']
      bs2'  = [(x, e'') | (x, e') <- bs', e'' <- maybeToList (maybeCanName e')]
      -- Each non-atomic RHS has a canonical name that is its LHS, the
      -- canonical name of each LHS with non-atomic RHS is itself, and
      -- for each LHS with atomic RHS, the canonical name is that RHS.
      env'  = [(e',    CanVar x) | (x, e') <- bs1']
           ++ [(Var x, CanVar x) | (x, _ ) <- bs1']
           ++ [(Var x, e'      ) | (x, e') <- bs2']
      body' = cseExpr (env' ++ env) body
cseExpr env (LetValues (Bindings bs) body)
    = mkLetValues bs''' body'
    where
      -- CSE each RHS
      bs'  = [(xs, cseExpr env e) | (xs, e) <- bs]
      -- Split the CSE'd bindings into 3 groups: those with the RHS of
      -- type Values but in which some of the components isn't atomic;
      -- those in which the RHS is of type Values and each component
      -- is atomic, and the rest (those in which the RHS is not of
      -- type Values).
      bs1' = [b' | b'@(_, Values es') <- bs', any isNotCanName es']
      bs2' = [b' | b'@(_, Values es') <- bs', all isCanName    es']
      bs'' = [b' | b'@(_, e') <- bs', isNotValues e']
      -- Each binding of the form ((x1 ... xn) (VALUES e1 ... en)),
      -- where each ei is atomic, implies that the canonical name of
      -- xi is ei.  Each such binding where some ei is not atomic
      -- implies that the canonical name of each ei is xi (this is not
      -- exactly right, but I don't have a better idea at the moment).
      -- Finally, each binding ((x1 ... xn) e), where e is not of type
      -- VALUES implies that each xi is its own canonical name.
      env'  = [(e',    CanVar x) | (xs, Values es') <- bs1', (x, e') <- zip xs es']
           ++ [(Var x, CanVar x) | (xs, _) <- bs''', x <- xs]
           ++ [(Var x, e''     ) | (xs, Values es') <- bs2'
                                 , let es'' = mapMaybe maybeCanName es'
                                 , (x, e'') <- zip xs es'']
      body' = cseExpr (env' ++ env) body
      -- Flush bindings with the RHS of type VALUES in which each
      -- component is atomic.
      bs''' = bs1' ++ bs''
cseExpr env (Car e)              = Car (cseExpr env e)
cseExpr env (Cdr e)              = Cdr (cseExpr env e)
cseExpr env (VectorRef e i)      = VectorRef (cseExpr env e) i
cseExpr env (Cons e1 e2)         = Cons (cseExpr env e1) (cseExpr env e2)
cseExpr env (Vector es)          = Vector (map (cseExpr env) es)
cseExpr env (Values es)          = Values (map (cseExpr env) es)
cseExpr env (ProcCall proc args) = ProcCall proc (map (cseExpr env) args)

cseDefn :: [(Expr, CanName)] -> Defn -> Defn
cseDefn env (Defn proc args body) = Defn proc args body'
    where
      body' = cseExpr (env' ++ env) body
      env'  = [(Var x, CanVar x) | (x, _) <- args]

cseProg :: [(Expr, CanName)] -> Prog -> Prog
cseProg env (Prog defns expr) = Prog (map (cseDefn env) defns) (cseExpr env expr)

cse :: Prog -> Prog
cse = cseProg []
