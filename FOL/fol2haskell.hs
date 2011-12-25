import FOL.Language.Common
import FOL.Language.Parser
import FOL.Language.Pretty

import FOL.Compiler.Haskell.CodeGen

compile :: String -> String
compile = pprint . compileProgAsModule (Name "Main") . parse

main = interact compile
