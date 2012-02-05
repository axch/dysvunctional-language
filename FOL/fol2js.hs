import FOL.Language.Common
import FOL.Language.Parser
import FOL.Language.Pretty

import FOL.Compiler.JavaScript.CodeGen

compile :: String -> String
compile = pprint . compileProg . parse

main = interact compile
