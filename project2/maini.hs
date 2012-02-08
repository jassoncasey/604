import System( getArgs )
import System.IO

import Interpreter
import Lexer
import CmdOpts

main = do
  putStr promptIntro
  untilQuit