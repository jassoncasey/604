import System( getArgs )
import Control.Monad( forM )
import System.IO

import CmdOpts



{- Project 2:
   In this project, the program shall print the AST of each valid program and
   its valuation.
-}
main = do
  args <- getArgs
  let (argsAreGood, errorMessage) = verifyArgs args
  if argsAreGood
    then do
      mapM putStrL $ args
    else
      putStr errorMessage