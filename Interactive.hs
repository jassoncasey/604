module Interactive
( startInteractive
) where

import System.IO
import Data.List( isPrefixOf )

import Lexer
import PrettyPrint
import Environment()

-- Fires up the interactive interpreter
startInteractive :: IO ()
startInteractive = do
  putStr promptIntro
  -- process coreLib
  replSpl (EnvState "Happy!")

-- Constant Strings --
promptStr :: String
promptStr = "> "
promptIntro :: String
promptIntro = "spli, the spl interpreter. For options, enter ':help'.\n"
promptHelp :: String
promptHelp = "Command options-\n"
  ++ "  :quit         Exit the interpreter.\n"
  ++ "  :help         Display command information.\n\n"
  ++ "  :lex s        Displays token information from lexed string s.\n"
  ++ "  :parse s      If parse is successful, 'valid' is displayed. Otherwise display\n"
  ++ "                the error.\n"
  ++ "  :environment  Displays current environment information\n"


-- Puts str in the output buffer and immediately flushes it to stdout handle
-- meaning, print str to screen immediately, avoids buffer overwrite
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Print prompt and get input
readPrompt :: IO String
readPrompt = flushStr promptStr >> getLine

isCmd :: String -> Bool
isCmd "" = False
isCmd s = ':' == (head $ dropWhile (==' ') s)

{-getCmd :: String -> (String,String)
getCmd s = (takeWhile (/=' ') $ tail $ dropWhile (==' ') s,"")-}

-- Carries the state of the environment
data EnvState = EnvState String deriving (Show)

-- evaluates until quit
replSpl :: EnvState -> IO ()
replSpl state = do
  input <- readPrompt
  if isCmd input
    then do
      let cmd = dropWhile (==' ') $ tail $ dropWhile (==' ') input
      if isPrefixOf "quit" cmd
        then putStrLn "Leaving SPLi."
        else if isPrefixOf "help" cmd
          then putStr promptHelp >> replSpl state
          else if isPrefixOf "lex" cmd
            then putStr (printTokenList $ tokenizeBuff "-" $ drop 3 cmd) >> replSpl state
            else putStr ("Unknown command: " ++ cmd ++ "\n") >> replSpl state
    else putStrLn input >> replSpl state

{-replSpl' :: EnvState -> IO ()
replSpl' state = do
  input <- readPrompt
  if isCmd input
    then do
      let (cmd, rest) = breakCmd input
      return $ case cmd of
        "quit"  -> putStrLn "Leaving SPL interactive."
        "help"  -> promptHelp >> replSpl' state
        "lex"   -> (printTokenList $ tokenizeBuff "-" rest) >> replSpl state
        "parse" -> putStrLn "Parse..." >> replSpl state
    else putStrLn input >> replState state-}