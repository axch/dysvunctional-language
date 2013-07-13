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
symbol = text

ppForm :: Pretty a => String -> [a] -> Doc
ppForm name xs = ppList $ symbol name : map pp xs

instance Pretty Name where
    pp (Name n) = symbol n
