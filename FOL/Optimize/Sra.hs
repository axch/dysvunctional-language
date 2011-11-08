{-# LANGUAGE NoImplicitPrelude, TypeOperators, MultiParamTypeClasses #-}
module FOL.Optimize.Sra where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty
import FOL.Language.TypeCheck
import FOL.Language.Unique

import Control.Applicative

data SraExpr
    = SraVar Name
    | SraNil
    | SraBool Bool
    | SraReal Real
    | SraIf SraExpr SraExpr SraExpr
    | SraLet (Bindings Name SraExpr) SraExpr
    | SraLetValues (Bindings [Name] SraExpr) SraExpr
    | SraValues [SraExpr]
    | SraProcCall Name [SraExpr]
      deriving (Eq, Show)

instance SraExpr :<: Expr where
    inj (SraVar x)  = Var x
    inj SraNil      = Nil
    inj (SraBool b) = Bool b
    inj (SraReal r) = Real r
    inj (SraIf p c a) = If (inj p) (inj c) (inj a)
    inj (SraLet (Bindings bs) body) = mkLet bs' (inj body)
        where
          bs' = [(x,  inj e) | (x,  e) <- bs]
    inj (SraLetValues (Bindings bs) body) = mkLetValues bs' (inj body)
        where
          bs' = [(xs, inj e) | (xs, e) <- bs]
    inj (SraValues es) = Values (map inj es)
    inj (SraProcCall proc args) = ProcCall proc (map inj args)

data SraDefn = SraDefn ShapedName [ShapedName] SraExpr
               deriving (Eq, Show)

instance SraDefn :<: Defn where
    inj (SraDefn proc args body) = Defn proc args (inj body)

data SraProg = SraProg [SraDefn] SraExpr
               deriving (Eq, Show)

instance SraProg :<: Prog where
    inj (SraProg defns expr) = Prog (map inj defns) (inj expr)

annShape :: Shape -> Unique (AnnShape Name)
annShape NilSh          = liftA  AnnNilSh    (uniqueName "sra")
annShape RealSh         = liftA  AnnRealSh   (uniqueName "sra")
annShape BoolSh         = liftA  AnnBoolSh   (uniqueName "sra")
annShape (ConsSh s1 s2) = liftA2 AnnConsSh   (annShape s1) (annShape s2)
annShape (VectorSh ss)  = liftA  AnnVectorSh (mapM annShape ss)
annShape (ValuesSh ss)  = liftA  AnnValuesSh (mapM annShape ss)

vars :: [Name] -> [SraExpr]
vars = map SraVar

values :: [Name] -> SraExpr
values [x] = SraVar x
values xs  = SraValues (vars xs)

mkSraLet :: [(Name, SraExpr)] -> SraExpr -> SraExpr
mkSraLet [] body = body
mkSraLet bs body = SraLet (Bindings bs) body

mkSraLetValues :: [([Name], SraExpr)] -> SraExpr -> SraExpr
mkSraLetValues [] body = body
mkSraLetValues bs body = SraLetValues (Bindings bs) body

smartSraLetValues :: [([Name], SraExpr)] -> SraExpr -> SraExpr
smartSraLetValues bs body = mkSraLet bs1 (mkSraLetValues bs2 body)
    where
      bs1 = [(x,  e) | ([x],        e) <- bs]
      bs2 = [(xs, e) | (xs@(_:_:_), e) <- bs]

sraExpr :: [(Name, AnnShape Name)] -> AnnExpr Type -> Unique SraExpr
sraExpr env (_, e) = sraExpr' env e

sraExpr' :: [(Name, AnnShape Name)] -> AnnExpr' Type -> Unique SraExpr
sraExpr' env (AnnVar x)
    | Just s <- lookup x env, let xs = annots s
    = return (values xs)
    | otherwise
    = error $ "Unbound variable: " ++ pprint x
sraExpr' _ AnnNil      = return SraNil
sraExpr' _ (AnnBool b) = return (SraBool b)
sraExpr' _ (AnnReal r) = return (SraReal r)
sraExpr' env (AnnIf p c a)
    = liftA3 SraIf (sraExpr env p) (sraExpr env c) (sraExpr env a)
sraExpr' env (AnnLet (Bindings bs) body)
    = do es' <- mapM (sraExpr env) es
         ss' <- mapM annShape ss
         let bs'  = zip xss es'
             xss  = map annots ss'
             env' = zip xs  ss'
         body' <- sraExpr (env' ++ env) body
         return $ smartSraLetValues bs' body'
    where
      (xs, es) = unzip bs
      ss       = [s | (PrimTy s, _) <- es]
sraExpr' env (AnnLetValues (Bindings bs) body)
    = do es'  <- mapM (sraExpr env) es
         sss' <- mapM (mapM annShape) sss
         let bs'  = zip xss' es'
             xss' = map concat xsss
             xsss = map (map annots) sss'
             env' = zip (concat xss) (concat sss')
         body' <- sraExpr (env' ++ env) body
         return $ smartSraLetValues bs' body'
    where
      (xss, es) = unzip bs
      sss       = [ss | (PrimTy (ValuesSh ss), _) <- es]
sraExpr' env (AnnCar e)
    = do e'  <- sraExpr env e
         s1' <- annShape s1
         s2' <- annShape s2
         let bs  = [(xs1 ++ xs2, e')]
             xs1 = annots s1'
             xs2 = annots s2'
         return $ smartSraLetValues bs (values xs1)
    where
      (PrimTy (ConsSh s1 s2), _) = e
sraExpr' env (AnnCdr e)
    = do e'  <- sraExpr env e
         s1' <- annShape s1
         s2' <- annShape s2
         let bs  = [(xs1 ++ xs2, e')]
             xs1 = annots s1'
             xs2 = annots s2'
         return $ smartSraLetValues bs (values xs2)
    where
      (PrimTy (ConsSh s1 s2), _) = e
sraExpr' env (AnnVectorRef e i)
    = do e' <- sraExpr env e
         ss' <- mapM annShape ss
         let bs  = [(concat xss, e')]
             xss = map annots ss'
         return $ smartSraLetValues bs (values (xss !! i))
    where
      (PrimTy (VectorSh ss), _) = e
sraExpr' env (AnnCons e1 e2)
    = do e1' <- sraExpr env e1
         e2' <- sraExpr env e2
         s1' <- annShape s1
         s2' <- annShape s2
         let bs  = [(xs1, e1'), (xs2, e2')]
             xs1 = annots s1'
             xs2 = annots s2'
         return $ smartSraLetValues bs (values (xs1 ++ xs2))
    where
      (PrimTy s1, _) = e1
      (PrimTy s2, _) = e2
sraExpr' env (AnnVector es)
    = do es' <- mapM (sraExpr env) es
         ss' <- mapM annShape ss
         let bs  = zip xss es'
             xss = map annots ss'
         return $ smartSraLetValues bs (values (concat xss))
    where
      ss  = [s | (PrimTy s, _) <- es]
sraExpr' env (AnnValues es)
    = do es' <- mapM (sraExpr env) es
         ss' <- mapM annShape ss
         let bs  = zip xss es'
             xss = map annots ss'
         return $ smartSraLetValues bs (values (concat xss))
    where
      ss  = [s | (PrimTy s, _) <- es]
sraExpr' env (AnnProcCall proc args)
    = do args' <- mapM (sraExpr env) args
         ss'   <- mapM annShape ss
         let bs  = zip xss args'
             xss = map annots ss'
         return $ smartSraLetValues bs (SraProcCall proc (vars (concat xss)))
    where
      ss  = [s | (PrimTy s, _) <- args]

-- The expression argument is expected to have shape (VALUES ss), with
-- the list ss parallel to the fringe of the shape argument.
shapeSraExpr :: AnnShape Name -> SraExpr -> Expr
shapeSraExpr _ SraNil      = Nil
shapeSraExpr _ (SraBool b) = Bool b
shapeSraExpr _ (SraReal r) = Real r
shapeSraExpr s e = smartLetValues [(annots s, inj e)] (exprOfShape s)

-- Given a shape with named primitive parts, construct (using variable
-- with those names) an expression that returns values of that shape
-- when the variables are bound to values of right type.
exprOfShape :: AnnShape Name -> Expr
exprOfShape (AnnNilSh  x)     = Var x
exprOfShape (AnnBoolSh x)     = Var x
exprOfShape (AnnRealSh x)     = Var x
exprOfShape (AnnConsSh s1 s2) = Cons (exprOfShape s1) (exprOfShape s2)
exprOfShape (AnnVectorSh ss)  = Vector (map exprOfShape ss)
exprOfShape (AnnValuesSh ss)  = Values (map exprOfShape ss)

sraDefn :: AnnDefn Type -> Unique SraDefn
sraDefn (_, AnnDefn proc args body)
    = do ss' <- mapM annShape ss
         let args' = concatMap fringe ss'
             env   = zip xs ss'
         body' <- sraExpr env body
         return $ SraDefn proc args' body'
    where
      (xs, ss) = unzip args

sraProg :: AnnProg Type -> Unique SraProg
sraProg (_, AnnProg defns expr)
    = liftA2 SraProg (mapM sraDefn defns) (sraExpr [] expr)

sra :: AnnProg Type -> Unique Prog
sra prog@(PrimTy prog_shape, _)
    = do SraProg defns' expr' <- sraProg prog
         prog_shape' <- annShape prog_shape
         return $ Prog (map inj defns') (shapeSraExpr prog_shape' expr')
