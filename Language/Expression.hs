{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable #-}
module FOL.Language.Expression where

import FOL.Language.Common
import FOL.Language.Pretty

import Data.Data
import Data.Monoid
import Data.Foldable (Foldable, foldMap)
import Data.Traversable

import Data.Generics.Uniplate.Data ()

import Control.Applicative

data Prog = Prog [Defn] Expr
            deriving (Eq, Show, Typeable, Data)

data Defn = Defn ShapedName [ShapedName] Expr
            deriving (Eq, Show, Typeable, Data)

data Shape
    = NilSh
    | RealSh
    | BoolSh
    | ConsSh Shape Shape
    | VectorSh [Shape]
    | ValuesSh [Shape]
      deriving (Eq, Ord, Show, Typeable, Data)

data Expr
    = Var Name
    -- Literals
    | Nil
    | Bool Bool
    | Real Real
    -- Special forms
    | If Expr Expr Expr
    | Let (Bindings Name Expr) Expr
    | LetValues (Bindings [Name] Expr) Expr
    -- Access
    | Car Expr
    | Cdr Expr
    | VectorRef Expr Int
    -- Construction
    | Cons Expr Expr
    | Vector [Expr]
    | Values [Expr]
    -- Procedure call
    | ProcCall Name [Expr]
      deriving (Eq, Show, Typeable, Data)

type ShapedName = (Name, Shape)

newtype Bindings p v = Bindings [(p, v)] deriving (Eq, Show, Typeable, Data)

instance Functor (Bindings p) where
    fmap f (Bindings bs)
        = Bindings [(x, f v) | (x, v) <- bs]

instance Foldable (Bindings p) where
    foldMap f (Bindings bs)
        = mconcat [f v | (_, v) <- bs]

instance Traversable (Bindings p) where
    traverse f (Bindings bs)
        = Bindings <$> sequenceA [liftA2 (,) (pure x) (f v) | (x, v) <- bs]

mkLet :: [(Name, Expr)] -> Expr -> Expr
mkLet [] body = body
mkLet bs body = Let (Bindings bs) body

mkLetValues :: [([Name], Expr)] -> Expr -> Expr
mkLetValues [] body = body
mkLetValues bs body = LetValues (Bindings bs) body

smartLetValues :: [([Name], Expr)] -> Expr -> Expr
smartLetValues bs body = mkLet bs1 (mkLetValues bs2 body)
    where
      bs1 = [(x,  e) | ([x],        e) <- bs]
      bs2 = [(xs, e) | (xs@(_:_:_), e) <- bs]

procName :: Defn -> Name
procName (Defn (proc_name, _) _ _) = proc_name

-- Pretty-printing of programs

instance Pretty Name where
    pp (Name n) = symbol n

instance Pretty Prog where
    pp (Prog []    expr) = pp expr
    pp (Prog defns expr) = parens $ symbol "begin" $+$ body
        where
          body = nest 1 (sepMap pp defns $+$ pp expr)

instance Pretty Defn where
    pp (Defn proc args body) = parens $ proto $+$ nest 1 (shapes $+$ pp body)
        where
          (proc_name, proc_shape) = proc
          (arg_names, arg_shapes) = unzip args
          proto  = symbol "define" <+> parens (pp proc_name <+> sepMap pp arg_names)
          shapes = ppForm "argument-types" (arg_shapes ++ [proc_shape])

instance Pretty Shape where
    pp NilSh          = ppList []
    pp RealSh         = symbol "real"
    pp BoolSh         = symbol "bool"
    pp (ConsSh t1 t2) = ppForm "cons"   [t1, t2]
    pp (VectorSh ts)  = ppForm "vector" ts
    pp (ValuesSh ts)  = ppForm "values" ts

instance Pretty Expr where
    pp Nil          = ppList []
    pp (Var x)      = pp x
    pp (Real r)     = real r
    pp (Bool True)  = symbol "#t"
    pp (Bool False) = symbol "#f"

    pp (If p c a) = parens $ symbol "if" <+> sepMap pp [p, c, a]

    pp (Let (Bindings bs) e)
        = parens $ (symbol "let" <+> bindings) $+$ (nest 1 body)
        where
          body = pp e
          bindings = parens . sepMap ppBinding $ bs
          ppBinding (name, expr) = ppList [pp name, pp expr]

    pp (LetValues (Bindings bs) e)
        = parens $ (symbol "let-values" <+> bindings) $+$ (nest 1 body)
        where
          body = pp e
          bindings = parens . sepMap ppBinding $ bs
          ppBinding (names, expr) = ppList [ppList (map pp names), pp expr]

    pp (Car e)         = ppForm "car" [e]
    pp (Cdr e)         = ppForm "cdr" [e]
    pp (VectorRef e i) = ppList [text "vector-ref", pp e, int i]
    pp (Cons e1 e2)    = ppForm "cons"   [e1, e2]
    pp (Vector es)     = ppForm "vector" es
    pp (Values es)     = ppForm "values" es

    pp (ProcCall proc args) = ppList $ pp proc : map pp args

-- Annotated programs, definitions, and expressions

-- It is really obnoxious that I have to write down these definitions
-- and pretty trivial conversion functions but there appears to be no
-- better way to arrange these things.  Cf.
--
-- Simon L. Peyton Jones, David Lester,
-- /A Modular Fully-lazy Lambda Lifter in Haskell/,
-- Software --- Practice and Experience, Vol. 21(5), 479--506 (1991).

type AnnProg ann = (ann, AnnProg' ann)
data AnnProg' ann
    = AnnProg [AnnDefn ann] (AnnExpr ann)
      deriving (Eq, Show)

stripAnnProg :: AnnProg ann -> Prog
stripAnnProg (_, prog) = stripAnnProg' prog

stripAnnProg' :: AnnProg' ann -> Prog
stripAnnProg' (AnnProg defns expr)
    = Prog (map stripAnnDefn defns) (stripAnnExpr expr)

instance Pretty (AnnProg' ann) where
    pp = pp . stripAnnProg'

type AnnDefn ann = (ann, AnnDefn' ann)
data AnnDefn' ann
    = AnnDefn ShapedName [ShapedName] (AnnExpr ann)
      deriving (Eq, Show)

stripAnnDefn :: AnnDefn ann -> Defn
stripAnnDefn (_, defn) = stripAnnDefn' defn

stripAnnDefn' :: AnnDefn' ann -> Defn
stripAnnDefn' (AnnDefn proc args body)
    = Defn proc args (stripAnnExpr body)

instance Pretty (AnnDefn' ann) where
    pp = pp . stripAnnDefn'

type AnnExpr ann = (ann, AnnExpr' ann)
data AnnExpr' ann
    = AnnVar Name
    | AnnNil
    | AnnBool Bool
    | AnnReal Real
    | AnnIf (AnnExpr ann) (AnnExpr ann) (AnnExpr ann)
    | AnnLet (Bindings Name (AnnExpr ann)) (AnnExpr ann)
    | AnnLetValues (Bindings [Name] (AnnExpr ann)) (AnnExpr ann)
    | AnnCar (AnnExpr ann)
    | AnnCdr (AnnExpr ann)
    | AnnVectorRef (AnnExpr ann) Int
    | AnnCons (AnnExpr ann) (AnnExpr ann)
    | AnnVector [AnnExpr ann]
    | AnnValues [AnnExpr ann]
    | AnnProcCall Name [AnnExpr ann]
      deriving (Eq, Show)

stripAnnExpr :: AnnExpr ann -> Expr
stripAnnExpr (_, e) = stripAnnExpr' e

stripAnnExpr' :: AnnExpr' ann -> Expr
stripAnnExpr' (AnnVar x)  = Var x
stripAnnExpr' AnnNil      = Nil
stripAnnExpr' (AnnBool b) = Bool b
stripAnnExpr' (AnnReal r) = Real r
stripAnnExpr' (AnnIf p c a)
    = If (stripAnnExpr p) (stripAnnExpr c) (stripAnnExpr a)
stripAnnExpr' (AnnLet bindings body)
    = Let (fmap stripAnnExpr bindings) (stripAnnExpr body)
stripAnnExpr' (AnnLetValues bindings body)
    = LetValues (fmap stripAnnExpr bindings) (stripAnnExpr body)
stripAnnExpr' (AnnCar e)              = Car (stripAnnExpr e)
stripAnnExpr' (AnnCdr e)              = Cdr (stripAnnExpr e)
stripAnnExpr' (AnnVectorRef e i)      = VectorRef (stripAnnExpr e) i
stripAnnExpr' (AnnCons e1 e2)         = Cons (stripAnnExpr e1) (stripAnnExpr e2)
stripAnnExpr' (AnnVector es)          = Vector (map stripAnnExpr es)
stripAnnExpr' (AnnValues es)          = Values (map stripAnnExpr es)
stripAnnExpr' (AnnProcCall proc args) = ProcCall proc (map stripAnnExpr args)

instance Pretty (AnnExpr' ann) where
    pp = pp . stripAnnExpr'

-- Annotated shapes

data AnnShape ann
    = AnnNilSh  ann
    | AnnRealSh ann
    | AnnBoolSh ann
    | AnnConsSh (AnnShape ann) (AnnShape ann)
    | AnnVectorSh [AnnShape ann]
    | AnnValuesSh [AnnShape ann]
      deriving (Eq, Show)

stripAnnShape :: AnnShape ann -> Shape
stripAnnShape (AnnNilSh  _)     = NilSh
stripAnnShape (AnnRealSh _)     = RealSh
stripAnnShape (AnnBoolSh _)     = BoolSh
stripAnnShape (AnnConsSh s1 s2) = ConsSh (stripAnnShape s1) (stripAnnShape s2)
stripAnnShape (AnnVectorSh ss)  = VectorSh (map stripAnnShape ss)
stripAnnShape (AnnValuesSh ss)  = ValuesSh (map stripAnnShape ss)

fringe :: AnnShape ann -> [(ann, Shape)]
fringe (AnnNilSh  ann)   = [(ann, NilSh )]
fringe (AnnRealSh ann)   = [(ann, RealSh)]
fringe (AnnBoolSh ann)   = [(ann, BoolSh)]
fringe (AnnConsSh s1 s2) = fringe s1 ++ fringe s2
fringe (AnnVectorSh ss)  = concatMap fringe ss
fringe (AnnValuesSh ss)  = concatMap fringe ss

annots :: AnnShape ann -> [ann]
annots = map fst . fringe
