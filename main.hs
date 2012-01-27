import System( getArgs)
import Control.Monad
import System.IO
import Lexer
import PrettyPrint
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
