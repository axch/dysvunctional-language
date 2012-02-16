{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable #-}

-- This module defines the types of 'let-lifted' FOL programs.  All
-- programs emitted by the FOL optimizer are 'let-lifted'.  At some
-- point this module should replace FOL.Language.Expression.

module FOL.Compiler.JavaScript.Expression where

import FOL.Language.Common
import FOL.Language.Pretty

import Control.Applicative

import Data.Data
import Data.Monoid
import Data.Foldable (Foldable, foldMap)
import Data.Traversable

data Prog = Prog [Defn] Blck
            deriving (Eq, Show, Typeable, Data)

data Defn = Defn ShapedName [ShapedName] Blck
            deriving (Eq, Show, Typeable, Data)

data Blck
    = Expr Expr
    | Let (Bindings Name Expr) Blck
    | LetValues (Bindings [Name] Expr) Blck
      deriving (Eq, Show, Typeable, Data)

data Expr
    = Var Name
    -- Literals
    | Nil
    | Bool Bool
    | Real Real
    -- Special forms
    | If Expr Blck Blck
    -- Access
    | Car Expr
    | Cdr Expr
    | VectorRef Expr Int
    -- Construction
    | Cons Expr Expr
    | Vector [Expr]
    | Values [Expr]
    | Lambda Name Blck
    -- Procedure call
    | ProcCall Name [Expr]
      deriving (Eq, Show, Typeable, Data)

data Shape
    = NilSh
    | RealSh
    | BoolSh
    | ConsSh Shape Shape
    | VectorSh [Shape]
    | ValuesSh [Shape]
    -- This is a hack.
    | FunctionSh
      deriving (Eq, Ord, Show, Typeable, Data)

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

instance Pretty Blck where
    pp (Expr e) = pp e
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
          ppBinding (names, expr) = parens $ ppList [ppList (map pp names), pp expr]

instance Pretty Expr where
    pp Nil                  = ppList []
    pp (Var x)              = pp x
    pp (Real r)             = real r
    pp (Bool True)          = symbol "#t"
    pp (Bool False)         = symbol "#f"
    pp (If p c a)           = parens $ symbol "if" <+> sep [pp p, pp c, pp a]
    pp (Car e)              = ppForm "car" [e]
    pp (Cdr e)              = ppForm "cdr" [e]
    pp (VectorRef e i)      = ppList [symbol "vector-ref", pp e, int i]
    pp (Cons e1 e2)         = ppForm "cons"   [e1, e2]
    pp (Vector es)          = ppForm "vector" es
    pp (Values es)          = ppForm "values" es
    pp (Lambda x e)         = ppList [symbol "lambda", parens (pp x), pp e]
    pp (ProcCall proc args) = ppList $ pp proc : map pp args

instance Pretty Shape where
    pp NilSh          = ppList []
    pp RealSh         = symbol "real"
    pp BoolSh         = symbol "bool"
    pp (ConsSh t1 t2) = ppForm "cons"   [t1, t2]
    pp (VectorSh ts)  = ppForm "vector" ts
    pp (ValuesSh ts)  = ppForm "values" ts
    pp FunctionSh     = symbol "escaping-function"
