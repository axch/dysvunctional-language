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
