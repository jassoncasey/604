import System( getArgs)
import Control.Monad
import System.IO
import Tokenizer
import PrettyPrint
import CmdOpts  -- for command-line work



-- TokenizedStatements

main = do
  args <- getArgs
  let (argsGood, errorMessage) = verifyArgs args
  if argsGood
    then do
      sources <- forM args readFile
      mapM putStrLn $ map (\(x,y) -> printTokenization x y) $ zip args sources
      return ()
    else putStr errorMessage
