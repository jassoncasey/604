import System( getArgs)
import Control.Monad
import System.IO
import Tokenizer
import Data.Maybe as Maybe
import Data.List as List
import PrettyPrint
import CmdOpts  -- for command-line work
import ListAux  -- For splitAfter
import Parse



-- TokenizedStatements

main = do
  args <- getArgs
  -- needs if to guard against empty
  -- content <- readFile $ head args
  let (argsGood, errorMessage) = verifyArgs args
  if argsGood
    then do
      fileContents <- forM args readFile
      mapM putStr $ map (\(x,y) -> printParse x y) (zip args fileContents)
      
      --print $ tokenStatements (head args) (head fileContents)
      --mapM putStrLn fileContents
      --mapM print $ tokenizeToStatements fileContents
      return ()
    else putStr errorMessage
  -- verify that there are arguments
  -- verify that each argument is an spl file
  --print $ tokenize (head args) 21 content