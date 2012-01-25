module Parser (
   parse
) where

import Lexer as Lexer

data Expression = Id Lexer.Token
                  | Nat Lexer.Token
                  | Let Lexer.Token Expression
                  | Lamda Lexer.Token Expression
                  | Unary Lexer.Token Expression
                  | Binary Lexer.Token Expression Expression
                  | Comp [Expression]
data Statement = Stmt Expression
data Program   = Prog [Statement]

-- main function of the parser
parseImp :: [Lexer.Token] -> ( Program, String )
parseImp (h:tl) = ( (:ast), ++ msg )
   where ( ast, msg ) = parseImp tl
parseImp [] = ([], "")

-- parse a filename and string buffer to a representation and output msgs
parse :: String -> String -> ( Program, String )
parse fname buf = 
   let tokens = Lexer.tokenizeBuff fname buf
   in parseImp tokens
