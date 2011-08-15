{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Common
    ( Real
    , Name (..)
    , primitives
    , module Prelude
    )
    where

import Prelude hiding (Real)

type Real = Double

data Name = Name String deriving (Eq, Ord, Show)

primitives :: [Name]
primitives
    = map Name [ "abs", "exp", "log"
               , "sin", "cos", "tan"
               , "asin", "acos", "sqrt", "real"
               , "+" , "-", "*", "/", "atan", "expt"
               , "zero?", "positive?", "negative?"
               , "<", "<=", ">", ">=", "="
               ]
