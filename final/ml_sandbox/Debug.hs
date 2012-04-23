module Steve.Debug where

import Text.Parsec.Prim

import Steve.Internal
import Steve.Parser
import Steve.Ast
import Steve.TypeCheck




testParser :: String -> [Declaration]
testParser input =
  case parse (parseUnit ["cow"]) "steve" input of
    Left err  -> error $ show err
    Right val -> val

parseFile :: String -> [String] -> IO ()
parseFile fname reservedNames = do
  source <- readFile fname
  let result = parse (parseUnit reservedNames) fname source
  case result of
    Left err -> putStrLn $ show err
    Right output -> case processDecls output of
      Just (expr, _,_, ctors) -> putStrLn $ show $ typeCheck ctors expr
      Nothing -> putStrLn "Failed to parse."
