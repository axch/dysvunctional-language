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

{-# LANGUAGE RecordWildCards #-}
import FOL.Language.Common
import FOL.Language.Parser
import FOL.Language.Pretty

import FOL.Compiler.Haskell.CodeGen

import Control.Applicative

import System.Console.GetOpt
import System.Environment
import System.Exit
import Data.Maybe(fromMaybe)

data Conf = Conf
  { moduleName :: String
  , exportName :: String
  , directory :: Maybe String
  } deriving Show

defaultConf :: Conf
defaultConf = Conf
  { moduleName = abort "You must specify a module name"
  , exportName = abort "You must specify an exported function name"
  , directory = Nothing
  }

usage :: String
usage = usageInfo header options
  where
    header = "Usage: fol2hs [OPTIONS...] FILE"

abort :: String -> a
abort msg = error $ msg ++ "\n\n" ++ usage

options :: [OptDescr (Conf -> IO Conf)]
options =
  [ Option ['h'] ["help"]
      (NoArg (\_ -> putStrLn usage >> exitWith ExitSuccess))
      "help"
  , Option ['m'] ["module"]
      (ReqArg (\m conf -> return conf { moduleName = m }) "NAME")
      "module NAME"
  , Option ['e'] ["export"]
      (ReqArg (\e conf -> return conf { exportName = e }) "NAME")
      "exported function NAME"
  , Option ['d'] ["directory"]
      (OptArg (\d conf -> return conf { directory = d }) "PATH")
      "output directory PATH"
  ]

main :: IO ()
main = do
  args <- getArgs
  (Conf {..}, input) <- case getOpt Permute options args of
    (o, [file], []    ) -> (,) <$> parseOpts o <*> readFile file
    (o, [],     []    ) -> (,) <$> parseOpts o <*> getContents
    (_, _,      []    ) -> abort "Too many input files"
    (_, _,      errors) -> abort $ concat errors
  let output = pprint
             . compileProgAsModule (Name moduleName)
                                   (Name exportName)
             . parse
             $ input
      outputFile = (fromMaybe "" directory) ++ moduleName ++ ".hs"
  writeFile outputFile output
    where
      parseOpts = foldl (>>=) (return defaultConf)
