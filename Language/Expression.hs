module FOL.Language.Expression where

type Name = String

data Prog = Prog [Defn] Expr deriving (Eq, Show)

data Defn = Defn TypedVar [TypedVar] Expr deriving (Eq, Show)

type TypedVar = (Name, Type)

data Type = TyNil
          | TyReal
          | TyBool
          | TyCons Type Type
          | TyVector [Type]
          | TyValues [Type]
            deriving (Eq, Show)

data Expr = Var Name
          -- Literals
          | Number Double | Boolean Bool | Nil
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

type LetBinding = (Name, Expr)
type LetValuesBinding = ([Name], Expr)
