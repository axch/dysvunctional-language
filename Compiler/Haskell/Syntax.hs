{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Compiler.Haskell.Syntax where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Data.List

data HsModule
    = HsModule Name [Name] [HsSCDefn]
      deriving Show

data HsSCDefn
    = HsSCDefn HsFunType Name [Name] HsExpr
      deriving Show

data HsBasicType
    = HsUnitTy
    | HsBoolTy
    | HsRealTy
    | HsPairTy HsBasicType HsBasicType
      deriving Show

type HsArgType = HsBasicType

data HsRetType
    = HsSingleValue HsBasicType
    | HsMultipleValues [HsBasicType]
      deriving Show

data HsFunType
    = HsFunType [HsArgType] HsRetType
      deriving Show

data HsExpr
    = HsVar Name
    | HsUnit
    | HsBool Bool
    | HsReal Real
    | HsIf HsExpr HsExpr HsExpr
    | HsLet (Bindings HsPat HsExpr) HsExpr
    | HsFst HsExpr
    | HsSnd HsExpr
    | HsPair HsExpr HsExpr
    | HsUnboxedTuple [HsExpr]
    | HsFuncAppl Name [HsExpr]
      deriving Show

data HsPat
    = HsPatVar Name
    | HsPatTuple [Name]
      deriving Show

ppTuple :: [Doc] -> Doc
ppTuple = parens . sep . punctuate comma

ppUnboxedTuple :: [Doc] -> Doc
ppUnboxedTuple = hashedParens . sep . punctuate comma
    where
      hashedParens x = text "(# " <> x <> text " #)"

newline :: Doc
newline = char '\n'

instance Pretty HsModule where
    pp (HsModule name exported_names sc_defns)
        = vcat $ punctuate newline (module_decl : map pp sc_defns)
        where
          module_decl = hsep [ text "module"
                             , pp name
                             , ppTuple (map pp exported_names)
                             , text "where" ]

instance Pretty HsSCDefn where
    pp (HsSCDefn sc_type sc_name sc_args sc_body)
        = sc_sign $+$ sc_defn
        where
          sc_sign = pp sc_name <+> text "::" <+> pp sc_type
          lhs = hsep (pp sc_name : map pp sc_args)
          rhs = pp sc_body
          sc_defn = lhs <+> equals <+> rhs

instance Pretty HsBasicType where
    pp HsUnitTy         = text "()"
    pp HsBoolTy         = text "Bool"
    pp HsRealTy         = text "Double#"
    pp (HsPairTy t1 t2) = ppTuple [pp t1, pp t2]

instance Pretty HsRetType where
    pp (HsSingleValue t)     = pp t
    pp (HsMultipleValues ts) = ppUnboxedTuple (map pp ts)

instance Pretty HsFunType where
    pp (HsFunType arg_types ret_type)
        = hsep . intersperse arrow $ map pp arg_types ++ [pp ret_type]
        where
          arrow = text "->"

instance Pretty HsExpr where
    pp (HsVar x) = pp x
    pp HsUnit = text "()"
    pp (HsBool True) = text "True"
    pp (HsBool False) = text "False"
    pp (HsReal d) = double d <> text "##"
    pp (HsIf p c a) = vcat [ text "if"   <+> pp p
                           , text "then" <+> pp c
                           , text "else" <+> pp a ]
    pp (HsLet (Bindings bs) body)
        = (text "let" <+> bindings) $+$ (text "in" <+> pp body)
        where
          bindings
              = braces . sep . punctuate semi $
                [pp p <+> equals <+> pp e | (p, e) <- bs]
    pp (HsFst e) = text "fst" <+> pp e
    pp (HsSnd e) = text "snd" <+> pp e
    pp (HsPair e1 e2) = ppTuple [pp e1, pp e2]
    pp (HsUnboxedTuple es) = ppUnboxedTuple $ map pp es
    pp (HsFuncAppl f xs) = sep $ pp f : map pp xs

instance Pretty HsPat where
    pp (HsPatVar x) = pp x
    pp (HsPatTuple xs) = char '!' <> ppUnboxedTuple (map pp xs)