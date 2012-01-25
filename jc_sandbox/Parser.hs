module Parser (
   parse
) where

import Lexer as Lexer

parseImp :: [Lexer.Token] -> String
parseImp (h:tl) = parseImp tl
parseImp [] = ""

-- parse a filename and string buffer to a list of tokens
parse :: String -> String -> String
parse fname buf = 
   let tokens = Lexer.tokenizeBuff fname buf
   in parseImp tokens
