import System( getArgs )
import Control.Monad( forM )
import Data.Maybe( isJust, fromJust )

import Lexing( lexString )
import Parsing( Program(..), parse_program )
import Ast( program )
--import Evaluate ( evaluate )
import Typing



main :: IO ()
main = do
  args <- getArgs
  testDiag args


-- print out information for diagnostics
testDiag :: [String] -> IO ()
testDiag args = do
  sources <- forM args readFile
  let lexedSources = map (\(a,b) -> lexString a b) $ zip args sources
  let (Program parsedSource, err) = parse_program $ foldl (++) [] lexedSources
  let ast = program (Program parsedSource)
  putStrLn "\n---------------- Lexing ----------------"
  _ <- ($) mapM putStrLn $ map show lexedSources
  putStrLn "\n\n---------------- Parsing ---------------"
  putStrLn ">>>> Diagnostics:"
  putStrLn $ show err
  putStrLn ">>>> Parse tree:"
  putStrLn $ show parsedSource
  putStrLn "\n\n------------------ AST -----------------"
  putStrLn $ show ast
  putStrLn "\n\n-------------- Evaluation --------------"
  putStrLn "   Sorry, folks. Out of Order."
  putStrLn "                                 -MGMT"
  {-if Maybe.isJust ast
    then do
      putStrLn $ show $ evaluate $ Maybe.fromJust ast
    else putStrLn "[]"-}
  putStrLn "\n\n---------------- Typing ----------------"
  if Data.Maybe.isJust ast
    then do
      putStrLn $ printType $ decltype $ Data.Maybe.fromJust ast
      putStr "\n"
    else putStrLn "[]"
  return ()