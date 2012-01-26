module Parse
( tokenStatements
, indexedLines
, tokenLines
) where

import Tokenizer
import ListAux



-- Indexes file contents, yields a list of tuples (line, String)
indexedLines :: String -> [(Int, String)]
indexedLines s = zip [1..(length s)] (lines s)

-- Maps an indexed sourcefile to its token form. Each list element corresponds
-- to a line in the source file.
tokenLines :: String -> String -> [[Token]]
tokenLines fn source = map (\(n,s) -> tokenize fn n s) $ indexedLines source

tokenizedSource :: String -> String -> [Token]
tokenizedSource fn source = foldr (++) [] $ tokenLines fn source

-- Takes a string of source code, cuts it up into statements, meaning lists of tokens
tokenStatements :: String -> String -> [[Token]]
tokenStatements fn source =
  splitAfter (not.isTerminalToken) $ tokenizedSource fn source



isTerminalToken :: Token -> Bool
isTerminalToken (Token_terminal _) = True
isTerminalToken _ = False

