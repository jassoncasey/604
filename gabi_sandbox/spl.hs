import System( getArgs )
import Control.Monad( forM )
import qualified Maybe

import Lexing( lexString )
import Parsing( Program(..), parse_program )
import Ast( program )
import Evaluate ( evaluate )



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
  putStrLn "---------------- Lexing ----------------"
  _ <- ($) mapM putStrLn $ map show lexedSources
  putStrLn "---------------- Parsing ---------------"
  putStrLn ">>>> Diagnostics:"
  putStrLn $ show err
  putStrLn ">>>> Parse tree:"
  putStrLn $ show parsedSource
  putStrLn "------------------ AST -----------------"
  putStrLn $ show ast
  putStrLn "-------------- Evaluation --------------"
  if Maybe.isJust ast
    then do
      putStrLn $ show $ evaluate $ Maybe.fromJust ast
    else putStrLn "[]"
  return ()