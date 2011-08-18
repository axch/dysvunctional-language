{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Expression where

import FOL.Language.Common
import FOL.Language.Pretty

data Prog = Prog [Defn] Expr deriving (Eq, Show)

data Defn = Defn ShapedName [ShapedName] Expr deriving (Eq, Show)

data Shape
    = NilSh
    | RealSh
    | BoolSh
    | ConsSh Shape Shape
    | VectorSh [Shape]
    | ValuesSh [Shape]
      deriving (Eq, Ord, Show)

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
    | VectorRef Expr Int
    -- Construction
    | Cons Expr Expr
    | Vector [Expr]
    | Values [Expr]
    -- Procedure call
    | ProcCall Name [Expr]
      deriving (Eq, Show)

type ShapedName       = (Name,   Shape)
type LetBinding       = (Name,   Expr)
type LetValuesBinding = ([Name], Expr)

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
          proto  = symbol "define" <+> (parens $ pp proc_name <+> sepMap pp arg_names)
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

    pp (Let bs e)
        = parens $ (symbol "let" <+> bindings) $+$ (nest 1 body)
        where
          body = pp e
          bindings = parens . sepMap ppBinding $ bs
          ppBinding (name, expr) = ppList [pp name, pp expr]

    pp (LetValues bs e)
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
