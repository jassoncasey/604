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

-- parse an IdTok into a Id
parseId :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseId (h:tl) = (tl, Id h, "")

-- parse a NatTok into a Nat
parseNat :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseNat (h:tl) = (tl, Nat h, "")

parseTerm :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseTerm 

-- attempt to parse a (LetTok:tl) into a Let
parseLet :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseLet (l:i:e:tl) =
   if ( Lexer.isToken i IdTok ) && ( Lexer.isToken e EqTok )
      then ( , Let l i e )
      else ( , Error, "Bad let\n" )

-- attempt to parse a (LamdaTok:IdTok:DotTok:tl) into a Lamda
parseLamda :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseLamda (l:i:d:tl) =

-- attempt to parse a (LParen:tl) into a Complex
parseComplex :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseComplex (h:tl) =

-- parse the terms
parseTerm :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseTerm tokens = 
   case headTok remainder of
      PlusTok  -> ( remainder, )
      MinusTok -> let (remainder', term, msg') = parseTerm (drop 1 remainder)
                  in (remainder', (Binary (head remainder), factor, term), "")
      _        -> ( remainder, factor, msg )
   where ( remainder, factor, msg)  = parseFactor tokens

-- break out the recognizable expression categories
parseExpr :: [Lexer.Token] -> ([Lexer.Token], Expression, String)
parseExpr tokens =
   let (Tok id _:tl) = tokens in
   case id of
      LetTok      -> parseLet tokens
      LamdaTok    -> parseLamda tokens
      _           -> parseTerm tokens

-- parse a single statement
parseStmt :: [Lexer.Token] -> ([Lexer.Token], Statement, String)
parseStmt tokens = 
   if IsHeadTok remainder Lexer.SemiTok 
      then ( drop 1 remainder, Stmt expr, msg )
      else ( tokens, Error, "Missing semicolon\n" )
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
