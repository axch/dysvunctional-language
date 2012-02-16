import FOL.Language.Common
import FOL.Language.Pretty

import FOL.Compiler.JavaScript.Parser
import FOL.Compiler.JavaScript.CodeGen

compile :: String -> String
compile = pprint . compileProg . parse

main = interact compile
