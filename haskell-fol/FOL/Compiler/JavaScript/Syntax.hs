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
module FOL.Compiler.JavaScript.Syntax where

import FOL.Language.Common
import FOL.Language.Pretty

import Data.Maybe
import Data.Monoid -- hiding ((<>)) In GHC 7.0.4, Data.Monoid does not export (<>)

data JsProg = JsProg [JsDefn]
              deriving Show

data JsDefn = JsDefn Name [Name] JsBlck
              deriving Show

data JsBlck = JsBlck [JsStmt]
              deriving Show

data JsStmt = JsReturn JsExpr              -- return e;
            | JsVarAssignment  Name JsExpr -- x = e;
            | JsVarDeclaration Name JsExpr -- var x = e;
              deriving Show

data JsExpr = JsVar Name
            | JsNull
            | JsBool Bool
            | JsReal Real
            | JsIf JsExpr JsBlck JsBlck
            | JsArray [JsExpr]
            | JsAccess JsExpr Int
            | JsFunction [Name] JsBlck
            | JsFunctionCall JsExpr [JsExpr]
            | JsInfixOpApplication Op JsExpr JsExpr
              deriving Show

data Op = Add | Sub | Mul | Div
        | Eq | Ne | Ge | Le | Gt | Lt
          deriving Show

newline, questionMark :: Doc
newline      = char '\n'
questionMark = char '?'

ppTuple :: Pretty a => (a -> Doc) -> [a] -> Doc
ppTuple pp = sep . punctuate comma . map pp

-- Just _ <=> named, Nothing <=> anonymous
ppFunction :: Maybe Name -> [Name] -> JsBlck -> Doc
ppFunction name args body
    =     text "function"
      <+> maybe empty ppName name <> parens (ppTuple ppName args)
      $+$ pp body

instance Pretty JsProg where
    pp (JsProg defns)
        -- Declare a global variable $result that is used for
        -- capturing and subsequently destructuring multiple return
        -- values.  Because we are going to generate code for
        -- alpha-renamed FOL programs, $result cannot collide with
        -- any of the existing names.
        =     text "var $result;" <> newline
          $+$ (vcat . punctuate newline $ map pp defns)

instance Pretty JsDefn where
    pp (JsDefn name args body)
        = ppFunction (Just name) args body <> semi

instance Pretty JsBlck where
    pp (JsBlck stmts) = lbrace $+$ (nest 4 . vcat . map pp) stmts $+$ rbrace

instance Pretty JsStmt where
    pp (JsReturn e) = text "return" <+> pp e <> semi
    pp (JsVarAssignment  x e) = ppName x <+> equals <+> pp e <> semi
    pp (JsVarDeclaration x e)
        = text "var" <+> ppName x <+> equals <+> pp e <> semi

instance Pretty JsExpr where
    pp (JsVar x) = ppName x
    pp JsNull = text "null"
    pp (JsBool True) = text "true"
    pp (JsBool False) = text "false"
    pp (JsReal d) = double d
    -- 'if' can occur in the RHS of variable assignment, therefore it
    -- needs to be an expression, not a statement.
    pp (JsIf p c a)
        = sep [pp p, questionMark, pp (expr c), colon, pp (expr a)]
    pp (JsArray es) = brackets (ppTuple pp es)
    pp (JsAccess e i) = pp' e <> brackets (int i)
    pp (JsFunction args body) = ppFunction Nothing args body
    pp (JsFunctionCall func args) = pp' func <> parens (ppTuple pp args)
    pp (JsInfixOpApplication op e1 e2) = pp' e1 <+> pp op <+> pp' e2

-- Turn a block into an expression by making it a body of a function
-- of no arguments and immediately calling that function.
expr :: JsBlck -> JsExpr
expr block = JsFunctionCall (JsFunction [] block) []

pp' :: JsExpr -> Doc
pp' e@(JsIf _ _ _)                 = parens (pp e)
pp' e@(JsFunction _ _)             = parens (pp e)
pp' e@(JsFunctionCall _ _)         = parens (pp e)
pp' e@(JsInfixOpApplication _ _ _) = parens (pp e)
pp' e = pp e

instance Pretty Op where
    pp Add = text "+"
    pp Sub = text "-"
    pp Mul = text "*"
    pp Div = text "/"
    pp Eq  = text "=="  -- The args are guaranteed to be of right type by FOL
    pp Ne  = text "!="
    pp Ge  = text ">="
    pp Le  = text "<="
    pp Gt  = text ">"
    pp Lt  = text "<"

ppName :: Name -> Doc
ppName = pp . prepare

prepare :: Name -> Name
prepare (Name name)
    = Name $ fromMaybe (replaceDashes name) (lookup name jsPrimitives)
    where
      replaceDashes = map (\c -> if c == '-' then '_' else c)

jsPrimitives :: [(String, String)]
jsPrimitives
    =    [ (name, "Math." ++ name)
               | name <- [ "abs", "exp", "log", "sqrt"
                         , "sin", "cos", "tan"
                         , "asin", "acos", "atan" ] ]
      ++ [ ("expt"     , "Math.pow"  )
         , ("real"     , "real"      )
         , ("zero?"    , "isZero"    )
         , ("positive?", "isPositive")
         , ("negative?", "isNegative")
         ]
