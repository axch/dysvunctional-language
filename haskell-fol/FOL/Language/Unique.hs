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
