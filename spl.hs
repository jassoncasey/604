import System( getArgs )
import Control.Monad( forM )
import System.IO

import Lexer
import CmdOpts

{- Project 2:
   In this project, the program shall print the AST of each valid program and
   its valuation.
-}

help_listing = 
   ":q         quits interpreter\n" ++
   ":h         prints help menu\n" ++
   "otherwise  input your line of SPL"

printHelp :: IO () 
printHelp = do
   putStrLn help_listing
   interactive

process :: [String] -> IO ()
process input = do
   putStrLn ("typed: " ++ (head input))
   interactive

interactive :: IO ()
interactive = do 
   putStr ">"
   hFlush stdout
   input <- getLine
   case input of
      ":q" -> return ()
      ":h" -> printHelp
      otherwise -> process [input]

batch :: [String] -> IO ()
batch args = putStrLn "Done"

main :: IO ()
main = do
   args <- getArgs
   let ( mode, errorMessage ) = verifyArgs args
   case mode of
      Interactive -> interactive
      Batch -> batch args
      Error -> putStrLn errorMessage
