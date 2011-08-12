{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Common
    ( Real
    , Name (..)
    , module Prelude
    )
    where

import Prelude hiding (Real)

type Real = Double

data Name = Name String deriving (Eq, Show)
