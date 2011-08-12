{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Expression where

import FOL.Language.Common
import FOL.Language.Pretty

import Control.Applicative

import Data.Foldable
import Data.Monoid
import Data.Traversable

data Prog name = Prog [Defn name] (Expr name) deriving (Eq, Show)

data Defn name = Defn (Typed name) [Typed name] (Expr name) deriving (Eq, Show)

data Type
    = NilTy
    | RealTy
    | BoolTy
    | ConsTy Type Type
    | VectorTy [Type]
    | ValuesTy [Type]
      deriving (Eq, Show)

data Expr name
    = Var name
    -- Literals
    | Nil
    | Bool Bool
    | Real Real
    -- Special forms
    | If (Expr name) (Expr name) (Expr name)
    | Let [LetBinding name] (Expr name)
    | LetValues [LetValuesBinding name] (Expr name)
    -- Access
    | Car (Expr name)
    | Cdr (Expr name)
    | VectorRef (Expr name) Integer
    -- Construction
    | Cons (Expr name) (Expr name)
    | Vector [Expr name]
    | Values [Expr name]
    -- Procedure call
    | ProcCall name [Expr name]
      deriving (Eq, Show)

type Typed name            = (name,   Type     )
type LetBinding name       = (name,   Expr name)
type LetValuesBinding name = ([name], Expr name)

-- Pretty-printing of programs

instance Pretty Name where
    pp (Name n) = text n

instance Pretty name => Pretty (Prog name) where
    pp (Prog []    expr) = pp expr
    pp (Prog defns expr) = parens $ text "begin" $+$ body
        where
          body = nest 1 (sepMap pp defns $+$ pp expr)

instance Pretty name => Pretty (Defn name) where
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

instance Pretty name => Pretty (Expr name) where
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

-- Expression is a Functor
instance Functor Expr where
    fmap f = fmapExpr
        where
          -- fmapExprs :: [Expr a] -> [Expr b]
          fmapExprs = map fmapExpr

          -- fmapExpr :: Expr a -> Expr b
          fmapExpr (Var x)    = Var (f x)
          fmapExpr Nil        = Nil
          fmapExpr (Bool b)   = Bool b
          fmapExpr (Real r)   = Real r
          fmapExpr (If p c a) = If (fmapExpr p) (fmapExpr c) (fmapExpr a)
          fmapExpr (Let bindings body)
              = Let [(f x, fmapExpr e) | (x, e) <- bindings] (fmapExpr body)
          fmapExpr (LetValues bindings body)
              = LetValues [(map f xs, fmapExpr e) | (xs, e) <- bindings] (fmapExpr body)
          fmapExpr (Car e)              = Car (fmapExpr e)
          fmapExpr (Cdr e)              = Cdr (fmapExpr e)
          fmapExpr (VectorRef e i)      = VectorRef (fmapExpr e) i
          fmapExpr (Cons e1 e2)         = Cons (fmapExpr e1) (fmapExpr e2)
          fmapExpr (Vector es)          = Vector (fmapExprs es)
          fmapExpr (Values es)          = Values (fmapExprs es)
          fmapExpr (ProcCall proc args) = ProcCall (f proc) (fmapExprs args)

-- Expressions are Foldable
instance Foldable Expr where
    foldMap f = foldExpr
        where
          -- foldExprs :: Monoid m => [Expr a] -> m
          foldExprs = mconcat . map foldExpr

          -- foldExpr :: Monoid m => Expr a -> m
          foldExpr (Var x)    = f x
          foldExpr Nil        = mempty
          foldExpr (Bool _)   = mempty
          foldExpr (Real _)   = mempty
          foldExpr (If p c a) = foldExprs [p, c, a]
          foldExpr (Let bindings body)
              = mconcat [ f x `mappend` foldExpr e
                              | (x, e) <- bindings ]
                `mappend` foldExpr body
          foldExpr (LetValues bindings body)
              = mconcat [ mconcat (map f xs) `mappend` foldExpr e
                              | (xs, e) <- bindings ]
                `mappend` foldExpr body
          foldExpr (Car e)              = foldExpr e
          foldExpr (Cdr e)              = foldExpr e
          foldExpr (VectorRef e _)      = foldExpr e
          foldExpr (Cons e1 e2)         = foldExprs [e1, e2]
          foldExpr (Vector es)          = foldExprs es
          foldExpr (Values es)          = foldExprs es
          foldExpr (ProcCall proc args) = f proc `mappend` foldExprs args

-- Expressions are Traversable
instance Traversable Expr where
    traverse f = traverseExpr
        where
          -- traverseExprs :: [Expr a] -> f [Expr b]
          traverseExprs = sequenceA . map traverseExpr

          -- traverseExpr :: Expr a -> f (Expr b)
          traverseExpr (Var x)    = Var <$> f x
          traverseExpr Nil        = pure Nil
          traverseExpr (Bool b)   = pure (Bool b)
          traverseExpr (Real r)   = pure (Real r)
          traverseExpr (If p c a) = liftA3 If p' c' a'
                where
                  p' = traverseExpr p
                  c' = traverseExpr c
                  a' = traverseExpr a
          traverseExpr (Let bindings body)
              = liftA2 Let bindings' body'
              where
                bindings' = sequenceA
                            [ liftA2 (,) (f x) (traverseExpr e)
                                  | (x, e) <- bindings ]
                body'     = traverseExpr body
          traverseExpr (LetValues bindings body)
              = liftA2 LetValues bindings' body'
              where
                bindings' = sequenceA
                            [ liftA2 (,) (traverse f xs) (traverseExpr e)
                                  | (xs, e) <- bindings ]
                body'     = traverseExpr body
          traverseExpr (Car e) = Car <$> traverseExpr e
          traverseExpr (Cdr e) = Cdr <$> traverseExpr e
          traverseExpr (VectorRef e i)
              = liftA2 VectorRef (traverseExpr e) (pure i)
          traverseExpr (Cons e1 e2)
              = liftA2 Cons (traverseExpr e1) (traverseExpr e2)
          traverseExpr (Vector es) = Vector <$> traverseExprs es
          traverseExpr (Values es) = Values <$> traverseExprs es
          traverseExpr (ProcCall proc args)
              = liftA2 ProcCall (f proc) (traverseExprs args)

-- Definition is a Functor
instance Functor Defn where
    fmap f (Defn proc args body) = Defn proc' args' body'
        where
          (proc_name, proc_type) = proc
          proc' = (f proc_name, proc_type)
          args' = [ (f arg_name, arg_type) | (arg_name, arg_type) <- args ]
          body' = fmap f body

-- Definitions are Foldable
instance Foldable Defn where
    foldMap f (Defn proc args body)
        = f proc_name `mappend` mconcat [ f arg_name | (arg_name, _) <- args ]
                      `mappend` foldMap f body
        where
          (proc_name, _) = proc

-- Definitions are Traversable
instance Traversable Defn where
    traverse f (Defn proc args body) = liftA3 Defn proc' args' body'
        where
          (proc_name, proc_type) = proc
          proc' = liftA2 (,) (f proc_name) (pure proc_type)
          args' = sequenceA [ liftA2 (,) (f arg_name) (pure arg_type)
                                  | (arg_name, arg_type) <- args ]
          body' = traverse f body

-- Program is a Functor
instance Functor Prog where
    fmap f (Prog defns expr) = Prog defns' expr'
        where
          defns' = map (fmap f) defns
          expr'  = fmap f expr

-- Programs are Foldable
instance Foldable Prog where
    foldMap f (Prog defns expr)
        = mconcat (map (foldMap f) defns) `mappend` foldMap f expr

-- Programs are Traversable
instance Traversable Prog where
    traverse f (Prog defns expr) = liftA2 Prog defns' expr'
        where
          defns' = sequenceA (map (traverse f) defns)
          expr'  = traverse f expr