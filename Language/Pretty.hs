{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Pretty
    ( Pretty
    , pp
    , real
    , pprint
    , sepMap
    , ppList
    , symbol
    , ppForm
    , module Text.PrettyPrint
    )
    where

import FOL.Language.Common

import Data.Char

import Text.PrettyPrint

class Pretty a where
    pp :: a -> Doc

real :: Real -> Doc
real = double

pprint :: Pretty a => a -> String
pprint = render . pp

sepMap :: (a -> Doc) -> [a] -> Doc
sepMap f = sep . map f

ppList :: [Doc] -> Doc
ppList = parens . sep

symbol :: String -> Doc
symbol s = text (map toUpper s)

ppForm :: Pretty a => String -> [a] -> Doc
ppForm name xs = ppList $ symbol name : map pp xs
