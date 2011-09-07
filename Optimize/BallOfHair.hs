{-# LANGUAGE NoImplicitPrelude, TypeOperators, MultiParamTypeClasses #-}
module FOL.Optimize.Cse where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Control.Applicative
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
             | SymCons SymExpr SymExpr
             | SymVector [SymExpr]
             | SymValues [SymExpr]
             | SymProcCall Name [SymExpr]
               deriving Eq

instance SymExpr :<: Expr where
    inj (SymVar x)              = Var x
    inj SymNil                  = Nil
    inj (SymBool b)             = Bool b
    inj (SymReal r)             = Real r
    inj (SymIf p c a)           = If (inj p) (inj c) (inj a)
    inj (SymCar e)              = Car (inj e)
    inj (SymCdr e)              = Cdr (inj e)
    inj (SymVectorRef e i)      = VectorRef (inj e) i
    inj (SymCons e1 e2)         = Cons (inj e1) (inj e2)
    inj (SymVector es)          = Vector (map inj es)
    inj (SymValues es)          = Values (map inj es)
    inj (SymProcCall proc args) = ProcCall proc (map inj args)

maybeSymExpr :: Expr -> Maybe SymExpr
maybeSymExpr (Var x)              = Just (SymVar x)
maybeSymExpr Nil                  = Just SymNil
maybeSymExpr (Bool b)             = Just (SymBool b)
maybeSymExpr (Real r)             = Just (SymReal r)
maybeSymExpr (If p c a)           = liftA3 SymIf (maybeSymExpr p) (maybeSymExpr c) (maybeSymExpr a)
maybeSymExpr (Let _ _)            = Nothing
maybeSymExpr (LetValues _ _)      = Nothing
maybeSymExpr (Car e)              = liftA SymCar (maybeSymExpr e)
maybeSymExpr (Cdr e)              = liftA SymCdr (maybeSymExpr e)
maybeSymExpr (VectorRef e i)      = liftA2 SymVectorRef (maybeSymExpr e) (pure i)
maybeSymExpr (Cons e1 e2)         = liftA2 SymCons (maybeSymExpr e1) (maybeSymExpr e2)
maybeSymExpr (Vector es)          = liftA SymVector (mapM maybeSymExpr es)
maybeSymExpr (Values es)          = liftA SymValues (mapM maybeSymExpr es)
maybeSymExpr (ProcCall proc args) = liftA (SymProcCall proc) (mapM maybeSymExpr args)

instance CanName :<: SymExpr where
    inj (CanVar x)  = SymVar x
    inj CanNil      = SymNil
    inj (CanBool b) = SymBool b
    inj (CanReal r) = SymReal r

maybeCanName :: SymExpr -> Maybe CanName
maybeCanName (SymVar x)  = Just (CanVar x)
maybeCanName SymNil      = Just CanNil
maybeCanName (SymBool b) = Just (CanBool b)
maybeCanName (SymReal r) = Just (CanReal r)
maybeCanName _           = Nothing

isVariable, isConstant, isValues, isNotValues :: SymExpr -> Bool
isVariable (SymVar _)  = True
isVariable _           = False

isConstant SymNil      = True
isConstant (SymBool _) = True
isConstant (SymReal _) = True
isConstant _           = False

isValues (SymValues _) = True
isValues _             = False

isNotValues = not . isValues

isCanName, isNotCanName :: SymExpr -> Bool
isCanName    = isJust    . maybeCanName
isNotCanName = isNothing . maybeCanName

cseExpr :: [(SymExpr, CanName)] -> Expr -> (SymExpr, Expr)
cseExpr env e
    | Just se <- maybeSymExpr e, isVariable se, Just a <- lookup se env
    = (inj a, inj a)
    | Just se <- maybeSymExpr e, isConstant se, Just a <- maybeCanName se
    = (inj a, inj a)
cseExpr _ (Var x)
    = error $ "Unbound variable: " ++ pprint x
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
      bs' = [(x, e') | (x, True, e') <- zip3 xs dead es']
cseExpr env (LetValues (Bindings bs) body)
    = (sbody, mkLetValues bs'''' body')
    where
      -- CSE each RHS
      bs'  = [(xs, cseExpr env e) | (xs, e) <- bs]
      -- Split the CSE'd bindings into 3 groups: those with the RHS of
      -- type Values but in which some of the components isn't atomic;
      -- those in which the RHS is of type Values and each component
      -- is atomic, and the rest (those in which the RHS is not of
      -- type Values).
      bs1' = [b' | b'@(_, (SymValues ss, _)) <- bs', any isNotCanName ss]
      bs2' = [b' | b'@(_, (SymValues ss, _)) <- bs', all isCanName    ss]
      bs'' = [b' | b'@(_, (s, _)) <- bs', isNotValues s]
      -- Each binding of the form ((x1 ... xn) (VALUES e1 ... en)),
      -- where each ei is atomic, implies that the canonical name of
      -- xi is ei.  Each such binding where some ei is not atomic
      -- implies that the canonical name of each ei is xi (this is not
      -- exactly right, but I don't have a better idea at the moment).
      -- Finally, each binding ((x1 ... xn) e), where e is not of type
      -- VALUES implies that each xi is its own canonical name.
      env'  = [(s,    CanVar x) | (xs, (SymValues ss, _)) <- bs1', (x, s) <- zip xs ss]
           ++ [(SymVar x, CanVar x) | (xs, _) <- bs''', x <- xs]
           ++ [(SymVar x, n     ) | (xs, (SymValues ss, _)) <- bs2'
                                 , let ns = mapMaybe maybeCanName ss
                                 , (x, n) <- zip xs ns]
      (sbody, body') = cseExpr (env' ++ env) body
      -- Flush bindings with the RHS of type VALUES in which each
      -- component is atomic.
      bs''' = bs1' ++ bs''
      bs'''' = [(xs, e') | (xs, (_, e')) <- bs''']
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

cseDefn :: [(SymExpr, CanName)] -> Defn -> Defn
cseDefn env (Defn proc args body) = Defn proc args body'
    where
      (_, body') = cseExpr (env' ++ env) body
      env'  = [(SymVar x, CanVar x) | (x, _) <- args]

cseProg :: [(SymExpr, CanName)] -> Prog -> Prog
cseProg env (Prog defns expr) = Prog (map (cseDefn env) defns) (snd (cseExpr env expr))

cse :: Prog -> Prog
cse = cseProg []

canonical :: [(SymExpr, CanName)] -> SymExpr -> SymExpr
canonical env symbolic
    | isInScope env candidate
    = candidate
    | otherwise
    = symbolic
    where
      candidate = fromMaybe symbolic (inj <$> lookup symbolic env)

emptyEnv :: [(SymExpr, CanName)]
emptyEnv = []

freshEnv :: [CanName] -> [(SymExpr, CanName)]
freshEnv ns = env
    where
      (env, _) = augmentEnv (map inj ns) ns emptyEnv

augmentEnv :: [SymExpr] -> [CanName] -> [(SymExpr, CanName)] -> ([(SymExpr, CanName)], [Bool])
augmentEnv ss ns env = (env' ++ env, map (isAcceptableAlias env) ss)
    where
      env' = concat (zipWith bind ss ns)
      bind s n
          | isAcceptableAlias env s, Just s' <- maybeCanName s
          = [(inj n, s')]
          | otherwise
          = (inj n, n) : if isVariable s then [] else [(s, n)]


isAcceptableAlias :: [(SymExpr, CanName)] -> SymExpr -> Bool
isAcceptableAlias env sym = isConstant sym || (isVariable sym && isInScope env sym)

isInScope :: [(SymExpr, CanName)] -> SymExpr -> Bool
isInScope env sym = isConstant sym || isJust (lookup sym env)
