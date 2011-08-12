{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Expression where

import FOL.Language.Common
import FOL.Language.Pretty

data Prog = Prog [Defn] Expr deriving (Eq, Show)

data Defn = Defn TypedName [TypedName] Expr deriving (Eq, Show)

data Type
    = NilTy
    | RealTy
    | BoolTy
    | ConsTy Type Type
    | VectorTy [Type]
    | ValuesTy [Type]
      deriving (Eq, Show)

data Expr
    = Var Name
    -- Literals
    | Nil
    | Bool Bool
    | Real Real
    -- Special forms
    | If Expr Expr Expr
    | Let [LetBinding] Expr
    | LetValues [LetValuesBinding] Expr
    -- Access
    | Car Expr
    | Cdr Expr
    | VectorRef Expr Integer
    -- Construction
    | Cons Expr Expr
    | Vector [Expr]
    | Values [Expr]
    -- Procedure call
    | ProcCall Name [Expr]
      deriving (Eq, Show)

type TypedName        = (Name,   Type)
type LetBinding       = (Name,   Expr)
type LetValuesBinding = ([Name], Expr)

-- Pretty-printing of programs

instance Pretty Name where
    pp (Name n) = text n

instance Pretty Prog where
    pp (Prog []    expr) = pp expr
    pp (Prog defns expr) = parens $ text "begin" $+$ body
        where
          body = nest 1 (sepMap pp defns $+$ pp expr)

instance Pretty Defn where
    pp (Defn proc args body) = parens $ proto $+$ nest 1 (types $+$ pp body)
        where
          (proc_name, proc_type) = proc
          (arg_names, arg_types) = unzip args
          proto = text "define" <+> (parens $ pp proc_name <+> sepMap pp arg_names)
          types = ppForm "argument-types" (arg_types ++ [proc_type])

instance Pretty Type where
    pp NilTy          = ppList []
    pp RealTy         = text "real"
    pp BoolTy         = text "bool"
    pp (ConsTy t1 t2) = ppForm "cons"   [t1, t2]
    pp (VectorTy ts)  = ppForm "vector" ts
    pp (ValuesTy ts)  = ppForm "values" ts

instance Pretty Expr where
    pp Nil          = ppList []
    pp (Var x)      = pp x
    pp (Real r)     = real r
    pp (Bool True)  = text "#t"
    pp (Bool False) = text "#f"

    pp (If p c a) = parens $ text "if" <+> sepMap pp [p, c, a]

    pp (Let bs e)
        = parens $ (text "let" <+> bindings) $+$ (nest 1 body)
        where
          body = pp e
          bindings = parens . sepMap ppBinding $ bs
          ppBinding (name, expr) = ppList [pp name, pp expr]

    pp (LetValues bs e)
        = parens $ (text "let-values" <+> bindings) $+$ (nest 1 body)
        where
          body = pp e
          bindings = parens . sepMap ppBinding $ bs
          ppBinding (names, expr) = ppList [ppList (map pp names), pp expr]

    pp (Car e)         = ppForm "car" [e]
    pp (Cdr e)         = ppForm "cdr" [e]
    pp (VectorRef e i) = ppList [text "vector-ref", pp e, integer i]
    pp (Cons e1 e2)    = ppForm "cons"   [e1, e2]
    pp (Vector es)     = ppForm "vector" es
    pp (Values es)     = ppForm "values" es

    pp (ProcCall proc args) = ppList $ pp proc : map pp args
