module Interpreter
( readPrompt
, untilQuit
, promptIntro
) where

import System.IO
import Data.List( isPrefixOf )


-- Constant Strings --
promptStr :: String
promptStr = "> "
promptIntro :: String
promptIntro = "spli, the spl interpreter. For options, enter ':help'.\n"
promptHelp :: String
promptHelp = "Command options-\n"
  ++ "  :quit   Exit the interpreter.\n"
  ++ "  :help   Display command information.\n"
  ++ "  :lex    Displays token information.\n"
  ++ "  :parse  If parse is successful, 'valid' is displayed. Otherwise display the error."



-- Puts str in the output buffer and immediately flushes it to stdout handle
-- meaning, print str to screen immediately, avoids buffer overwrite
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Print prompt and get input
readPrompt :: IO String
readPrompt = flushStr promptStr >> getLine

isCmd :: String -> Bool
isCmd s = ':' == (head $ dropWhile (==' ') s)

getCmd :: String -> (String,String)
getCmd s = (takeWhile (/=' ') $ tail $ dropWhile (==' ') s,"")

-- evaluates until quit
untilQuit :: IO ()
untilQuit = do
  input <- readPrompt
  if isCmd input
    then do
      let cmd = dropWhile (==' ') $ tail $ dropWhile (==' ') input
      if isPrefixOf "quit" cmd
        then putStrLn "Leaving SPLi."
        else if isPrefixOf "help" cmd
          then putStr promptHelp >> untilQuit
          else putStr ("Unknown command: " ++ cmd ++ "\n") >> untilQuit
    else putStrLn input >> untilQuit
  
