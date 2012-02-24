import System( getArgs )
import Control.Monad( forM )
import Data.Maybe( isJust, fromJust, isNothing )

import Lexing( lexString )
import Parsing( Program(..), parse_program )
import Ast( program )
--import Evaluate ( evaluate )
import Typing



main :: IO ()
main = do
  args <- getArgs
  _ <- forM args typeChecknPrint
  return ()
  -- Check args
  --testDiag args


-- typecheck the a file
typeChecknPrint :: String -> IO()
typeChecknPrint filename = do
  putStrLn ("Processing " ++ filename ++ ":")
  source <- readFile filename
  let tokens = lexString filename source
  let (Program parsedSource, err) = parse_program tokens
  if (not.null) err
    then putStrLn "  Error parsing file."
    else do
      let ast = program (Program parsedSource)
      if isNothing ast
        then putStrLn "  Error extracting ast from source."
        else do
          let ty = decltype $ fromJust ast
          if isJust ty
            then putStrLn $ printType $ fromJust ty
            else putStrLn "  Type error."


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
      putStrLn $ printType $ fromJust $ decltype $ fromJust ast
      putStr "\n"
    else putStrLn "    No type information: Failed to parse."
  return ()
