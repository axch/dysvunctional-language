{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Optimize.Optimize where

import FOL.Language.AlphaRn
import FOL.Language.Common
import FOL.Language.Parser
import FOL.Language.Pretty
import FOL.Language.Tidy
import FOL.Language.TypeCheck
import FOL.Language.Unique

import FOL.Optimize.Cse
import FOL.Optimize.Inline
import FOL.Optimize.Sra

import Control.Monad

optimize :: String -> String
optimize = pprint . tidy . evalUnique . transform . parse
    where
      transform = liftM cse . ((sra . ann) <=< inline <=< alphaRn)
