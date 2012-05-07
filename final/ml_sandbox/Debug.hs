module Steve.Debug where

import Text.Parsec.Prim

import Steve.Internal
import Steve.Parser
import Steve.Ast
import Steve.TypeCheck2




--testParser :: String -> [Declaration]
testParser parser' input =
  case parse parser' "steve" input of
    Left err  -> error $ show err
    Right val -> val

--test :: String -> IO ()
test parser' fname = do
  source <- readFile fname
  case parse parser' fname source of
    Left err -> putStrLn $ show err
    Right out -> putStrLn $ show out

parseFile :: String -> IO ()
parseFile fname = do
  source <- readFile fname
  let result = parse (parseUnit []) fname source
  case result of
    Left err -> putStrLn $ show err
    Right output -> putStrLn $ show output


getAstFromFile :: String -> IO ()
getAstFromFile fname = do
  source <- readFile fname
  let result = parse (parseUnit []) fname source
  case result of
    Left err -> putStrLn $ show err
    Right output -> putStrLn $ show $ processDecls output

fileToAst :: String -> IO ()
fileToAst fname = do
  source <- readFile fname
  let result = parse (parseUnit []) fname source
  case result of
    Left err -> putStrLn $ show err
    Right output -> putStrLn $ show $ toAsts output

typeCheckFile :: String -> IO ()
typeCheckFile fname = do
  source <- readFile fname
  let result = parse (parseUnit []) fname source
  case result of
    Left err -> putStrLn $ show err
    Right output -> case toAsts output of
      Just asts -> putStrLn $ show $ topLevelChecker ([],gammaInit) asts
      Nothing -> putStrLn "Failed to parse."

{-typeCheckFileM1 :: String -> IO ()
typeCheckFileM1 fname = do
  source <- readFile fname
  let result = parse (parseUnit []) fname source
  case result of
    Left err -> putStrLn $ show err
    Right output -> case processDecls output of
      Just (expr, _,_, ctors) -> putStrLn $ show $ typeCheckM1 ctors expr
      Nothing -> putStrLn "Failed to parse."

typeCheckFileM2 :: String -> IO ()
typeCheckFileM2 fname = do
  source <- readFile fname
  let result = parse (parseUnit []) fname source
  case result of
    Left err -> putStrLn $ show err
    Right output -> case processDecls output of
      Just (expr, _,_, ctors) -> putStrLn $ show $ typeCheckM2 ctors expr
      Nothing -> putStrLn "Failed to parse."

typeCheckFileM3 :: String -> IO ()
typeCheckFileM3 fname = do
  source <- readFile fname
  let result = parse (parseUnit []) fname source
  case result of
    Left err -> putStrLn $ show err
    Right output -> case processDecls output of
      Just (expr, _,_, ctors) -> putStrLn $ show $ typeCheckM3 ctors expr
      Nothing -> putStrLn "Failed to parse."-}