{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Unique where

import FOL.Language.Common

import Control.Monad.State

type Unique = State Int

uniqueName :: String -> Unique Name
uniqueName prefix = do i <- get
                       let name = prefix ++ show i
                       put (succ i)
                       return (Name name)