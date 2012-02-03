import System( getArgs )

import CmdOpts
import Interactive

{- Project 2:
   In this project, the program shall print the AST of each valid program and
   its valuation.
-}

batch :: [String] -> IO ()
batch _ = putStrLn "Done"

main :: IO ()
main = do
   args <- getArgs
   let ( mode, errorMessage ) = verifyArgs args
   case mode of
      Interactive -> startInteractive
      Batch -> batch args
      Error -> putStrLn errorMessage
