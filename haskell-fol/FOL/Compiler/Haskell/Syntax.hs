-- ----------------------------------------------------------------------
-- Copyright 2010-2011 National University of Ireland.
-- ----------------------------------------------------------------------
-- This file is part of DysVunctional Language.
-- 
-- DysVunctional Language is free software; you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
--  License, or (at your option) any later version.
-- 
-- DysVunctional Language is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU Affero General Public License
-- along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
-- ----------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Compiler.Haskell.Syntax where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Data.List
import Data.Maybe

data HsModule
    = HsModule [HsPragma] Name [Name] [HsImport] [HsTyDefn] [HsSCDefn] [String]
      deriving Show

-- An algebraic data type would be more appropriate, but we only need
-- a place to stick a few language extensions, so a string suffices.
data HsPragma
    = HsPragma String
      deriving Show

data HsImport
    = HsImport String
      deriving Show

data HsTyDefn
    = HsTyDefn HsShape
      deriving Show

data HsSCDefn
    = HsSCDefn HsType Name [Name] HsExpr
      deriving Show

data HsShape
    = HsUnitSh
    | HsBoolSh
    | HsBoxedDoubleSh
    | HsUnboxedDoubleSh
    | HsPairSh HsShape HsShape
    | HsUnboxedTupleSh [HsShape]
    | HsFunctionSh
      deriving Show

data HsType
    = HsPrimType HsShape
    | HsFuncType [HsShape] HsShape
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
    | HsLambda Name HsExpr
    | HsUnboxedTuple [HsExpr]
    | HsFuncAppl Name [HsExpr]
      deriving Show

data HsPat
    = HsPatVar Name
    | HsPatTuple [Name]
      deriving Show

ppTuple :: [Doc] -> Doc
ppTuple = parens . sep . punctuate comma

-- The GHC cannot handle arbitrarily long unboxed tuples (the maximum
-- supported size is 62 on my 32 bit machine).  However, as of 7.6.3,
-- unboxed tuples can be nested (an unboxed tuple has kind #).
maxTupleSize :: Int
maxTupleSize = 62

ppUnboxedTuple :: [Doc] -> Doc
ppUnboxedTuple xs = ppUnboxedTuple' xs (length xs)
    where
      k = maxTupleSize - 1

      ppUnboxedTuple' xs n =
        hashedParens . sep . punctuate comma $
          if n <= maxTupleSize
          then xs
          else let (xs', xs'') = splitAt k xs
               in xs' ++ [ppUnboxedTuple' xs'' (n - k)]

      hashedParens x = text "(# " <> x <> text " #)"

newline :: Doc
newline = char '\n'

instance Pretty HsModule where
    pp (HsModule pragmas name exported_names imports type_defns sc_defns extras)
        = vcat (pragma_decls : module_body)
        where
          pragma_decls = vcat (map pp pragmas)
          import_decls = vcat (map pp imports)
          type_decls   = vcat (map pp type_defns)
          module_decl  = hsep [ text "module"
                              , ppName name
                              , ppTuple (map ppName exported_names)
                              , text "where" ]
          module_body  = punctuate newline
                         ( module_decl
                         : import_decls
                         : type_decls
                         : map pp sc_defns) ++ map text extras

instance Pretty HsPragma where
    pp (HsPragma pragma) = text pragma

instance Pretty HsImport where
    pp (HsImport module_name) = text "import" <+> text module_name

instance Pretty HsTyDefn where
    pp (HsTyDefn shape)
        = text "newtype" <+> text "Function" <+> equals <+> lhs
        where
          lhs =     text "Function"
                <+> (parens $ sep [text "Double#", arrow, pp shape])
          arrow = text "->"

instance Pretty HsSCDefn where
    pp (HsSCDefn sc_type sc_name sc_args sc_body)
        = sc_sign $+$ sc_defn
        where
          sc_sign = ppName sc_name <+> text "::" <+> pp sc_type
          lhs = hsep (ppName sc_name : map ppName sc_args)
          rhs = pp sc_body
          sc_defn = hang lhs 4 (equals <+> rhs)

instance Pretty HsShape where
    pp HsUnitSh              = text "()"
    pp HsBoolSh              = text "Bool"
    pp HsBoxedDoubleSh       = text "Double"
    pp HsUnboxedDoubleSh     = text "Double#"
    pp (HsPairSh t1 t2)      = ppTuple [pp t1, pp t2]
    pp (HsUnboxedTupleSh ts) = ppUnboxedTuple (map pp ts)
    pp HsFunctionSh          = text "Function"

instance Pretty HsType where
    pp (HsPrimType shape) = pp shape
    pp (HsFuncType arg_types ret_type)
        = hsep . intersperse arrow . map pp $ arg_types ++ [ret_type]
        where
          arrow = text "->"

instance Pretty HsExpr where
    pp (HsVar x) = ppName x
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
    pp (HsLambda x e) = parens $ sep [backslash, ppName x, arrow, pp e]
        where
          backslash = char '\\'
          arrow = text "->"
    pp (HsUnboxedTuple es) = ppUnboxedTuple $ map pp es
    pp (HsFuncAppl f xs) = parens . sep $ ppName f : map pp xs

haskellPrimitives :: [(String, String)]
haskellPrimitives
    = [ ("abs"      , "absDouble#" )
      , ("exp"      , "expDouble#" )
      , ("log"      , "logDouble#" )
      , ("sin"      , "sinDouble#" )
      , ("cos"      , "cosDouble#" )
      , ("tan"      , "tanDouble#" )
      , ("asin"     , "asinDouble#")
      , ("acos"     , "acosDouble#")
      , ("atan"     , "atanDouble#")
      , ("sqrt"     , "sqrtDouble#")
      , ("real"     , "real"       )
      , ("+"        , "(+##)"      )
      , ("-"        , "(-##)"      )
      , ("*"        , "(*##)"      )
      , ("/"        , "(/##)"      )
      , ("expt"     , "(**##)"     )
      , ("zero?"    , "isZero#"    )
      , ("positive?", "isPositive#")
      , ("negative?", "isNegative#")
      , ("<"        , "(<##)"      )
      , ("<="       , "(<=##)"     )
      , (">"        , "(>##)"      )
      , (">="       , "(>=##)"     )
      , ("="        , "(==##)"     )
      ]

prepare :: Name -> Name
prepare (Name name)
    = Name $ fromMaybe (replaceDashes name) (lookup name haskellPrimitives)
    where
      replaceDashes = map (\c -> if c == '-' then '_' else c)

ppName :: Name -> Doc
ppName = pp . prepare

bang :: Doc
bang = char '!'

instance Pretty HsPat where
    pp (HsPatVar x) = bang <> ppName x
    pp (HsPatTuple xs) = bang <> ppUnboxedTuple (map ppName xs)

prelude :: [HsSCDefn]
prelude = [ HsSCDefn (HsFuncType [HsUnboxedDoubleSh] HsUnboxedDoubleSh)
                     (Name "absDouble#")
                     [x]
                     (HsIf (HsFuncAppl (Name "(>=##)") [ HsVar x
                                                       , HsReal 0.0 ])
                           (HsVar x)
                           (HsFuncAppl (Name "negateDouble#") [HsVar x]))
          , HsSCDefn (HsFuncType [HsUnboxedDoubleSh] HsUnboxedDoubleSh)
                     (Name "real")
                     [x]
                     (HsVar x)
          , HsSCDefn (HsFuncType [HsUnboxedDoubleSh] HsBoolSh)
                     (Name "isZero#")
                     [x]
                     (HsFuncAppl (Name "(==##)") [HsVar x, HsReal 0.0])
          , HsSCDefn (HsFuncType [HsUnboxedDoubleSh] HsBoolSh)
                     (Name "isPositive#")
                     [x]
                     (HsFuncAppl (Name "(>##)") [HsVar x, HsReal 0.0])
          , HsSCDefn (HsFuncType [HsUnboxedDoubleSh] HsBoolSh)
                     (Name "isNegative#")
                     [x]
                     (HsFuncAppl (Name "(<##)") [HsVar x, HsReal 0.0])
          ]
    where
      x = Name "x"
