module Parser (
   parse
) where

import Lexer as Lexer

-- Simple Programming Language (SPL) parse representation
data Expression = Id Lexer.Token
                  | Nat Lexer.Token
                  | Let Lexer.Token Expression
                  | Lamda Lexer.Token Expression
                  | Unary Lexer.Token Expression
                  | Binary Lexer.Token Expression Expression
                  | Comp [Expression]
                  deriving (Show)
data Statement = Stmt Expression deriving (Show)
data Program   = Prog [Statement] deriving (Show)

-- parse a single statement
parseStmt :: [Lexer.Token] -> ([Lexer.Token], Statement, String)
parseStmt = 
parseStmt [] = ( None, "" )

-- simple statement collector
parseStmts :: [Lexer.Token] -> ([Statement], String)
parseStmts tks = ( (stmt:stmts), msg++msgs )
   where ( remainder, stmt, msg ) = parseStmt tks
         (stmts, msgs) = parseStmts remainder
parseStmts [] = [] 

-- simple program parser creator
parseProgram :: [Lexer.Token] -> ( Program, String )
parseProgram tks = ( Prog stmts, msgs )
   where ( stmts, msgs ) = parseStatements tks
parseProgram [] = ( Prog [], "" )

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
