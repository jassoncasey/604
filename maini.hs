import System( getArgs )
import Control.Monad( forever )
import System.IO

import Interpreter
import Lexer
import CmdOpts


interIntro :: String
interIntro = "spli, the spl interpreter. For options, enter ':help'.\n"

interHelp :: String
interHelp = "Command options-\n"
  ++ "  :quit - exit the interpreter\n"
  ++ "  :help - display command information\n"

isCmd :: String -> Bool
isCmd s = ':' == (head $ dropWhile (==' ') s)

getCmd :: String -> (String,String)
getCmd s = (takeWhile (/=' ') $ tail $ dropWhile (==' ') s,"")

interPrompt :: String
interPrompt = "> "

{- Project 2:
   In this project, the program shall print the AST of each valid program and
   its valuation.
-}
main = do
  putStr promptIntro
  untilQuit