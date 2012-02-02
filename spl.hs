import System( getArgs )
--import Control.Monad( forM )
import System.IO

--import Lexer
import CmdOpts
import Interactive

{- Project 2:
   In this project, the program shall print the AST of each valid program and
   its valuation.
-}

batch :: [String] -> IO ()
batch args = putStrLn "Done"

main :: IO ()
main = do
   args <- getArgs
   let ( mode, errorMessage ) = verifyArgs args
   case mode of
      Interactive -> startInteractive
      Batch -> batch args
      Error -> putStrLn errorMessage
