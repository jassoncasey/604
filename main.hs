import System( getArgs)
import Control.Monad
import System.IO
import Tokenizer2
import PrettyPrint2
import CmdOpts
--import Lexer

main = do
  args <- getArgs
  let (argsAreGood, errorMessage) = verifyArgs args
  if argsAreGood
    then do
      sources <- forM args readFile
      mapM putStrLn $ map (\(x,y) -> printTokenization x y) $ zip args sources
      return ()
    else putStr errorMessage
