import System( getArgs )
import Control.Monad( forM )
import Data.Maybe( isJust, fromJust, isNothing )
import Data.List( isSuffixOf )

import Lexing( lexString )
import Parsing( Program(..), parse_program )
import Ast( program )
--import Evaluate ( evaluate )
import Typing
import ProofTree



main :: IO ()
main = do
  args <- getArgs
  let (mode, filenames) = processArgs args
  case mode of
    Batch -> typeCheck filenames
    AstDiag -> testDiag filenames
    ProofTree -> typeCheckTree filenames
    LatexProofTree -> texifyProofTrees filenames
    ArgError -> putStrLn "Error in arguments."
  return ()


-- typecheck the a file
typeCheck ::[String] -> IO()
typeCheck args = do
  _ <- forM args typeChecknPrint
  return ()
typeChecknPrint :: String -> IO()
typeChecknPrint filename = do
  putStrLn ("Typechecking " ++ filename ++ ":")
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
          let ty = typeTerm $ fromJust ast
          putStrLn $ getStrType $ type_ ty

typeCheckTree :: [String] -> IO()
typeCheckTree args = putStrLn "Not implemented yet."

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
      --putStrLn $ printType $ fromJust $ decltype $ fromJust ast
      putStr "\n"
    else putStrLn "    No type information: Failed to parse."
  return ()

processArgs :: [String] -> (SplMode, [String])
processArgs args' = case args' of
  (('-':cmd):args) -> case allSplFiles args of
    True -> ( modeFromStr cmd,args)
    False -> (ArgError, [])
  args -> case allSplFiles args of
    True -> (Batch,args)
    False -> (ArgError, [])
  where allSplFiles = all (isSuffixOf ".spl")


-- Tex the output
texifyProofTrees :: [String] -> IO()
texifyProofTrees args = do
  _ <- forM args texifyProofTree
  return ()

texifyProofTree :: String -> IO()
texifyProofTree filename = do
  source <- readFile filename
  let tokens = lexString filename source
  let (Program parsedSource, err) = parse_program tokens
  if (not.null) err
    then putStrLn ("Error parsing file: " ++ filename ++ ".\n\n")
    else do
      let ast = program (Program parsedSource)
      if isNothing ast
        then putStrLn ("Error building ast in file: " ++ filename ++ ".\n\n")
        else do
          let ty = typeTerm $ fromJust ast
          putStr $ texInDocTree ty



data SplMode =
    Batch
  | AstDiag
  | LatexProofTree
  | ProofTree
  | ArgError
modeFromStr :: String -> SplMode
modeFromStr cmd = case cmd of
  "" -> Batch
  "ast" -> AstDiag
  "tree" -> ProofTree
  "tex" -> LatexProofTree
  _ -> ArgError

