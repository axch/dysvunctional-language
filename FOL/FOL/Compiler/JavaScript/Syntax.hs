{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Compiler.JavaScript.Syntax where

import FOL.Language.Common
import FOL.Language.Expression
import FOL.Language.Pretty

import Data.List
import Data.Maybe

data JsProg = JsProg [JsDefn]
              deriving Show

data JsDefn = JsDefn Name [Name] JsExpr
              deriving Show

data JsExpr = JsVar Name
            | JsNull
            | JsBool Bool
            | JsReal Real
            | JsIf JsExpr JsExpr JsExpr
            | JsArray [JsExpr]
            | JsAccess JsExpr Int
            | JsFunction [Name] JsExpr
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
ppFunction :: Maybe Name -> [Name] -> JsExpr -> Doc
ppFunction name args body
    =     text "function" <+> maybe empty ppName name <> parens (ppTuple ppName args)
      $+$ lbrace
      $+$ nest 4 (text "return" <+> pp body <> semi)
      $+$ rbrace

instance Pretty JsProg where
    pp (JsProg defns)
        = vcat . punctuate newline $ map pp defns

instance Pretty JsDefn where
    pp (JsDefn name args body)
        = ppFunction (Just name) args body <> semi

instance Pretty JsExpr where
    pp (JsVar x) = ppName x
    pp JsNull = text "null"
    pp (JsBool True) = text "true"
    pp (JsBool False) = text "false"
    pp (JsReal d) = double d
    pp (JsIf p c a) = sep [pp p, questionMark, pp c, colon, pp a]
    pp (JsArray es) = brackets (ppTuple pp es)
    pp (JsAccess e i) = pp' e <> brackets (int i)
    pp (JsFunction args body) = ppFunction Nothing args body
    pp (JsFunctionCall func args) = pp' func <> parens (ppTuple pp args)
    pp (JsInfixOpApplication op e1 e2) = pp' e1 <+> pp op <+> pp' e2

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
