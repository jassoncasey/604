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
                  | Complex [Expression]
                  | Error
                  deriving (Show)
data Statement = Stmt Expression deriving (Show)
data Program   = Prog [Statement] deriving (Show)

-- peak at the head and validate its token id
isHeadTok :: [Lexer.Token] -> Lexer.TokId -> Bool
isHeadTok (h:tl) id =
   case h of 
      Tok value _ -> value == id
      _           -> False

parseExpr :: [Lexer.Token] -> ([Lexer.Token], Expression, String)
parseExpr tokens =
   let (Tok id _:tl) = tokens in
   case id of
      NatTok      -> parseNat tokens
      LetTok      -> parseLet tokens
      LamdaTok    -> parseLamda tokens
      LParenTok   -> parseComplex tokens
      _           ->

-- parse a single statement
parseStmt :: [Lexer.Token] -> ([Lexer.Token], Statement, String)
parseStmt tokens = 
   if IsHeadTok remainder Lexer.SemiTok 
      then ( drop 1 remainder, Stmt expr, msg )
      else ( remainder, Error, "Could not match expression\n" )
   where ( remainder, expr, msg ) = parseExpr tokens

-- simple statement collector
parseStmts :: [Lexer.Token] -> ([Statement], String)
parseStmts tokens = ( (stmt:stmts), msg++msgs )
   where ( remainder, stmt, msg ) = parseStmt tokens
         (stmts, msgs) = parseStmts remainder
parseStmts [] = [] 

-- simple program parser creator
parseProgram :: [Lexer.Token] -> ( Program, String )
parseProgram tokens = ( Prog stmts, msgs )
   where ( stmts, msgs ) = parseStatements tokens

-- main function of the parser
parseImp :: [Lexer.Token] -> ( Program, String )
parseImp tokens = parseProgram tokens
parseImp [] = ([], "")

-- parse a filename and string buffer to a representation and output msgs
parse :: String -> String -> ( Program, String )
parse fname buf = 
   let tokens = Lexer.tokenizeBuff fname buf
   in parseImp tokens
