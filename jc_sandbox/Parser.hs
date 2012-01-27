module Parser (
   parse,
   Expression(..),
   Statement(..),
   Program(..)
) where

import Lexer as Lexer

data MExpr = JExpr Expression
           | JNothing
           deriving (Show)

-- Simple Programming Language (SPL) parse representation
data Expression = Id Lexer.Token
                  | Nat Lexer.Token
                  | Let Lexer.Token Expression Lexer.Token Expression
                  | Lamda Lexer.Token Expression Lexer.Token Expression
                  | Unary Lexer.Token Expression
                  | Binary Lexer.Token Expression Expression
                  | Application Expression Expression
                  | Complex Lexer.Token [Expression] Lexer.Token
                  | ErrExpr
                  deriving (Show)
data Statement = Stmt Expression 
               | ErrStmt 
               deriving (Show)
data Program   = Prog [Statement]
               | ErrPrg
               deriving (Show)

-- trivial error formatting functions
getErrExpr (Id token) = Lexer.getErrHdr token
getErrExpr (Nat token) = Lexer.getErrHdr token
getErrExpr (Let token _ _ _) = Lexer.getErrHdr token
getErrExpr (Lamda token _ _ _) = Lexer.getErrHdr token
getErrExpr (Unary token _) = Lexer.getErrHdr token
getErrExpr (Binary _ exprl _) = getErrExpr exprl
getErrExpr (Application expr _) = getErrExpr expr
getErrExpr (Complex token _ _) = Lexer.getErrHdr token
getErrExpr _ = "Unknown location"

-- trivial code printing functions
getStrExpr (Id token) = Lexer.getLexeme token
getStrExpr (Nat token) = Lexer.getLexeme token
getStrExpr (Let _ dst _ src) = 
   "Let " ++ (getStrExpr dst) ++ " = " ++(getStrExpr src)
getStrExpr (Lamda _ param _ body) =
   "\\" ++ (getStrExpr param) ++ "." ++ (getStrExpr body)
getStrExpr (Unary op expr) =
   Lexer.getLexeme op ++ (getStrExpr expr)
getStrExpr (Binary op exprl exprr) =
   (getStrExpr exprl) ++ " " ++ (Lexer.getLexeme op) ++ " " ++ (getStrExpr exprr)
getStrExpr (Application exprl exprr) =
   (getStrExpr exprl) ++ (getStrExpr exprr)
getStrExpr (Complex _ exprl _ ) = ""
getStrExpr _ = ""
--   case exprr of
--     JExpr expr -> "( " ++ (getStrExpr exprl) ++ "; " 
--         ++ getStrExpr expr ++ " )"
--      JNothing -> "( " ++ (getStrExpr exprl) ++ " )"

-- peak at the head and validate its token id
isHeadTok :: [Lexer.Token] -> Lexer.TokId -> Bool
isHeadTok (Lexer.Tok value _:tl) id = value == id
isHeadTok tokens id = False

-- simple token id extraction
getTokId :: Lexer.Token -> Lexer.TokId
getTokId (Lexer.Tok id _) = id

-- parse an IdTok into a Id
parseId :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseId (h:tl) = (tl, Id h, "")

-- parse a NatTok into a Nat
parseNat :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseNat (h:tl) = (tl, Nat h, "")

-- attempt to parse a (LParen:tl) into a Complex
--parseComplex :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
--parseComplex (h:tl) =
--   let ( remainder, expression, msg ) = parseExpr tl
--   in case (head remainder) of 
--      Lexer.Tok Lexer.SemiTok _ -> 
--         let (remainder', exprs, msg) = parseComplex (drop 1 remainder)
--         in ( remainder', (Complex expression (JExpr exprs)), msg )
--      Lexer.Tok Lexer.RParenTok _ -> ( remainder, (Complex expression JNothing), msg )
--      _ -> ( remainder, ErrExpr, "Invalid Complex expression\n" )

--parseCompound :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
--parseCompound tokens =
--   if (Lexer.isToken h Lexer.LParen) || (Lexer.isToken h Lexer.SemiTok)
--      then let ( remainder, expr, msg ) = parseExpr tl
--              ( Lexer.Tok id _ ) = head remainder
--            in case id of
--               Lexer.RParenTok -> ( drop 1 remainder, Compound 
--               Lexer.SemiTok -> 
--               _ -> ( remainder, ErrExpr, "" )
--      else ( remainder, ErrExpr, "" )
--   where (h:tl) = tokens

-- attempt to parse the primary terms and non-term
parsePrimary :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parsePrimary tokens = 
   let (Lexer.Tok id _:tl) = tokens in
   case id of
      Lexer.IdTok -> parseId tokens
      Lexer.NatTok -> parseNat tokens
--      Lexer.LParenTok -> parseCompound tokens
--         let (remainder, exprs, msg) = parseComplex (drop 1 tokens)
--            in if Lexer.isToken (head remainder) Lexer.RParen
--                  then ( drop 1 remainder, Complex id exprs (head remainder))
--                  else ( remainder, ErrExpr, "" )
      _ -> ( tokens, ErrExpr, "Unknown primary\n" )

-- parse an application
parseApp :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseApp tokens =
   if length msg' == 0
         then ( remainder', Application primary app, "" )
         else ( remainder, primary, "")
   where ( remainder, primary, msg ) = parsePrimary tokens
         ( remainder', app, msg' ) = parseApp remainder

-- parse a factor
parseFactor :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseFactor tokens =
   case head remainder of
      Lexer.Tok Lexer.MultTok _ -> 
                  let (remainder', factor, msg') = parseFactor (drop 1 remainder)
                  in (remainder', (Binary (head remainder) app factor), "")
      Lexer.Tok Lexer.DivTok _  -> 
                  let (remainder', factor, msg') = parseFactor (drop 1 remainder)
                  in (remainder', (Binary (head remainder) app factor), "")
      _              -> ( remainder, app, msg )
   where ( remainder, app, msg)  = parseApp tokens

-- parse a term
parseTerm :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseTerm tokens = 
   case head remainder of
      Lexer.Tok Lexer.PlusTok  _ -> 
                  let (remainder', term, msg') = parseTerm (drop 1 remainder)
                  in (remainder', (Binary (head remainder) factor term), "")
      Lexer.Tok Lexer.MinusTok _ -> 
                  let (remainder', term, msg') = parseTerm (drop 1 remainder)
                  in (remainder', (Binary (head remainder) factor term), "")
      _        -> ( remainder, factor, msg )
   where ( remainder, factor, msg)  = parseFactor tokens

-- attempt to parse a (LetTok:tl) into a Let
parseLet :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseLet tokens =
   let (lt:id:eq:tl) = tokens
   in if ( Lexer.isToken id IdTok ) && ( Lexer.isToken eq EqTok )
      then let ( remainder, expr, msg ) = parseExpr (drop 3 tokens)
            in ( remainder, Let lt (Id id) eq expr, msg )
      else ( tokens, ErrExpr, "Bad let\n" )

-- attempt to parse a (LamdaTok:IdTok:DotTok:tl) into a Lamda
parseLamda :: [Lexer.Token] -> ( [Lexer.Token], Expression, String )
parseLamda tokens =
   let (lm:id:dt:tl) = tokens
   in if ( Lexer.isToken id IdTok ) && ( Lexer.isToken dt DotTok )
      then let ( remainder, expr, msg ) = parseExpr (drop 3 tokens)
            in ( remainder, Lamda lm (Id id) dt expr, msg )
      else ( tokens, ErrExpr, "Bad lamda\n" )

-- break out the recognizable expression categories
parseExpr :: [Lexer.Token] -> ([Lexer.Token], Expression, String)
parseExpr tokens =
   let (Lexer.Tok id _:tl) = tokens in
   case id of
      LetTok      -> parseLet tokens
      LamdaTok    -> parseLamda tokens
      _           -> parseTerm tokens

-- parse a single statement
parseStmt :: [Lexer.Token] -> ([Lexer.Token], Statement, String)
parseStmt tokens = 
   if isHeadTok remainder Lexer.SemiTok 
      then ( drop 1 remainder, Stmt expr, msg )
      else ( remainder, ErrStmt, "Syntax error: missing semicolon\n"
               ++ (getErrExpr expr) 
               ++ "\t" ++ (getStrExpr expr) ++ "\n")
   where ( remainder, expr, msg ) = parseExpr tokens

-- simple statement collector
parseStmts :: [Lexer.Token] -> ([Statement], String)
parseStmts (h:tl) = ( (stmt:stmts), msg++msgs )
   where ( remainder, stmt, msg ) = parseStmt (h:tl)
         (stmts, msgs) = parseStmts remainder
parseStmts [] = ([],"") 

-- simple program parser creator
parseProgram :: [Lexer.Token] -> ( Program, String )
parseProgram tokens = ( Prog stmts, msgs )
   where ( stmts, msgs ) = parseStmts tokens

-- main function of the parser
parseImp :: [Lexer.Token] -> ( Program, String )
parseImp (h:tl) = parseProgram (h:tl)
parseImp [] = (Prog [], "")

-- parse a filename and string buffer to a representation and output msgs
parse :: String -> String -> ( Program, String )
parse fname buf = 
   let tokens = Lexer.tokenizeBuff fname buf
   in parseImp tokens
