{-# LANGUAGE RecordWildCards #-}
import FOL.Language.Common
import FOL.Language.Parser
import FOL.Language.Pretty

import FOL.Compiler.Haskell.CodeGen

import Control.Applicative

import System.Console.GetOpt
import System.Environment
import System.Exit

data Conf = Conf
  { moduleName :: String
  , exportName :: String
  } deriving Show

defaultConf :: Conf
defaultConf = Conf
  { moduleName = abort "You must specify a module name"
  , exportName = abort "You must specify an exported function name"
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
      (NoArg (\conf -> putStrLn usage >> exitWith ExitSuccess))
      "help"
  , Option ['m'] ["module"]
      (ReqArg (\m conf -> return conf { moduleName = m }) "NAME")
      "module NAME"
  , Option ['e'] ["export"]
      (ReqArg (\e conf -> return conf { exportName = e }) "NAME")
      "exported function NAME"
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
      outputFile = moduleName ++ ".hs"
  writeFile outputFile output
    where
      parseOpts = foldl (>>=) (return defaultConf)
