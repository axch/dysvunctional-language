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

{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, TypeOperators, MultiParamTypeClasses #-}
module FOL.Language.Common
    ( Real
    , Name (..)
    , primitives
    , compose
    , (:<:) (..)
    , module Prelude
    )
    where

import Data.Data

import Prelude hiding (Real)

type Real = Double

data Name = Name String deriving (Eq, Ord, Show, Typeable, Data)

primitives :: [Name]
primitives
    = map Name [ "abs", "exp", "log"
               , "sin", "cos", "tan"
               , "asin", "acos", "sqrt", "real"
               , "+" , "-", "*", "/", "atan", "expt"
               , "zero?", "positive?", "negative?"
               , "<", "<=", ">", ">=", "="
               ]

compose :: [a -> a] -> a -> a
compose = foldr (.) id

class sub :<: sup where
    inj :: sub -> sup
