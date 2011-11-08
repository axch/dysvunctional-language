{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Unique where

import FOL.Language.Common

import Control.Monad.State

type Unique = State Int

evalUnique :: Unique a -> a
evalUnique = flip evalState 0

uniqueName :: String -> Unique Name
uniqueName prefix = do i <- get
                       let name = prefix ++ "-" ++ show i
                       put (succ i)
                       return (Name name)

-- type UniqueT = StateT Int

-- evalUniqueT :: Monad m => UniqueT m a -> m a
-- evalUniqueT = flip evalStateT 0

-- uniqueName :: Monad m => String -> UniqueT m Name
-- uniqueName prefix = do i <- get
--                        let name = prefix ++ "-" ++ show i
--                        put (succ i)
--                        return (Name name)
