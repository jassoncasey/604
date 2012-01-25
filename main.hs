import System( getArgs)
import System.IO
import Tokenizer
import Data.Maybe as Maybe
import Data.List as List
import TokenizerPrint
import CmdOpts  -- for command-line work
import ListAux  -- For splitAfter



main = do
  args <- getArgs
  -- needs if to guard against empty
  -- content <- readFile $ head args
  let (argsGood, errorMessage) = verifyArgs args
  if argsGood
    then do
      mapM putStrLn $ map (\x -> "Parsing " ++ x) args
      -- for each file
        -- print filename
        -- pf = parsed file into tokens
        -- statements = split on terminal
        -- 
      return ()
    else putStr errorMessage
  -- verify that there are arguments
  -- verify that each argument is an spl file
  --print $ tokenize (head args) 21 content