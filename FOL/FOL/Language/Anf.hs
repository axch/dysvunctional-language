{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}
module FOL.Language.Anf where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Unique

import qualified Data.Traversable as Traversable

import Control.Applicative
import Control.Monad.Writer

data AnfExpr
    = AnfSimpleExpr AnfSimpleExpr
    | AnfIf AnfExpr AnfExpr AnfExpr
    | AnfLet (Bindings Name AnfExpr) AnfExpr
    | AnfLetValues (Bindings [Name] AnfExpr) AnfExpr
    | AnfCar AnfSimpleExpr
    | AnfCdr AnfSimpleExpr
    | AnfVectorRef AnfSimpleExpr Int
    | AnfCons AnfSimpleExpr AnfSimpleExpr
    | AnfVector [AnfSimpleExpr]
    | AnfValues [AnfSimpleExpr]
    | AnfProcCall Name [AnfSimpleExpr]
      deriving (Eq, Show)

data AnfSimpleExpr
    = AnfVar Name
    | AnfNil
    | AnfBool Bool
    | AnfReal Real
      deriving (Eq, Show)

instance AnfSimpleExpr :<: Expr where
    inj (AnfVar x)  = Var x
    inj AnfNil      = Nil
    inj (AnfBool b) = Bool b
    inj (AnfReal r) = Real r

instance AnfExpr :<: Expr where
    inj (AnfSimpleExpr simple_expr) = inj simple_expr
    inj (AnfIf p c a) = If (inj p) (inj c) (inj a)
    inj (AnfLet (Bindings bs) body) = mkLet bs' (inj body)
        where
          bs' = [(x,  inj e) | (x,  e) <- bs]
    inj (AnfLetValues (Bindings bs) body) = mkLetValues bs' (inj body)
        where
          bs' = [(xs, inj e) | (xs, e) <- bs]
    inj (AnfCar e)              = Car (inj e)
    inj (AnfCdr e)              = Cdr (inj e)
    inj (AnfVectorRef e i)      = VectorRef (inj e) i
    inj (AnfCons e1 e2)         = Cons (inj e1) (inj e2)
    inj (AnfVector es)          = Vector (map inj es)
    inj (AnfValues es)          = Values (map inj es)
    inj (AnfProcCall proc args) = ProcCall proc (map inj args)

maybeSimpleExpr :: Expr -> Maybe AnfSimpleExpr
maybeSimpleExpr (Var x)  = Just (AnfVar x)
maybeSimpleExpr Nil      = Just AnfNil
maybeSimpleExpr (Bool b) = Just (AnfBool b)
maybeSimpleExpr (Real r) = Just (AnfReal r)
maybeSimpleExpr _        = Nothing

data AnfDefn = AnfDefn ShapedName [ShapedName] AnfExpr
               deriving (Eq, Show)

instance AnfDefn :<: Defn where
    inj (AnfDefn proc args body) = Defn proc args (inj body)

data AnfProg = AnfProg [AnfDefn] AnfExpr
               deriving (Eq, Show)

instance AnfProg :<: Prog where
    inj (AnfProg defns expr) = Prog (map inj defns) (inj expr)

type NormalT = WriterT [(Name, AnfExpr)]

normalize :: Expr -> NormalT Unique AnfSimpleExpr
normalize e
    | Just simple_expr <- maybeSimpleExpr e
    = return simple_expr
    | otherwise
    = do name <- lift $ uniqueName "anf"
         e'   <- lift $ anfExpr e
         tell [(name, e')]
         return (AnfVar name)

mkAnfLet :: [(Name, AnfExpr)] -> AnfExpr -> AnfExpr
mkAnfLet [] body = body
mkAnfLet bs body = AnfLet (Bindings bs) body

toAnfLet :: Monad m => NormalT m AnfExpr -> m AnfExpr
toAnfLet a = do (body, bindings) <- runWriterT a
                return $ mkAnfLet bindings body

anfExpr :: Expr -> Unique AnfExpr
anfExpr e
    | Just simple_expr <- maybeSimpleExpr e
    = return (AnfSimpleExpr simple_expr)

anfExpr (If p c a)
    = liftA3 AnfIf        (anfExpr p) (anfExpr c) (anfExpr a)
anfExpr (Let bindings body)
    = liftA2 AnfLet       (Traversable.mapM anfExpr bindings) (anfExpr body)
anfExpr (LetValues bindings body)
    = liftA2 AnfLetValues (Traversable.mapM anfExpr bindings) (anfExpr body)

anfExpr (Car e)
    = toAnfLet $ liftA  AnfCar       (normalize e)
anfExpr (Cdr e)
    = toAnfLet $ liftA  AnfCdr       (normalize e)
anfExpr (VectorRef e i)
    = toAnfLet $ liftA2 AnfVectorRef (normalize e) (pure i)
anfExpr (Cons e1 e2)
    = toAnfLet $ liftA2 AnfCons      (normalize e1) (normalize e2)
anfExpr (Vector es)
    = toAnfLet $ liftA  AnfVector    (mapM normalize es)
anfExpr (Values es)
    = toAnfLet $ liftA  AnfValues    (mapM normalize es)
anfExpr (ProcCall proc args)
    = toAnfLet $ liftA2 AnfProcCall  (pure proc) (mapM normalize args)

anfDefn :: Defn -> Unique AnfDefn
anfDefn (Defn proc args body) = liftA (AnfDefn proc args) (anfExpr body)

anfProg :: Prog -> Unique AnfProg
anfProg (Prog defns expr) = liftA2 AnfProg (mapM anfDefn defns) (anfExpr expr)

anf :: Prog -> Unique Prog
anf = liftA inj . anfProg
