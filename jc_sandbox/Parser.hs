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
                  | ENothing 
                  | ErrExpr
                  deriving (Show,Eq)
data Statement = Stmt Expression 
               | ErrStmt 
               deriving (Show,Eq)
data Program   = Prog [Statement]
               | ErrPrg
               deriving (Show,Eq)

data Status = Success | Failure deriving (Show,Eq)

getPositionInfo :: Expression -> String
getPositionInfo (Id token) = 
   "Filename: " ++ (Lexer.getFile token) ++ "\n" ++
   "Line: " ++ (Lexer.getLineNo token) ++ 
   " Colomn: " ++ (Lexer.getColStart token) ++
   " - Column: " ++ (show (Lexer.getColEnd token)) ++ "\n"
getPositionInfo (Nat token) =
   "Filename: " ++ (Lexer.getFile token) ++ "\n" ++
   "Line: " ++ (Lexer.getLineNo token) ++ 
   " Colomn: " ++ (Lexer.getColStart token) ++
   " - Column: " ++ (show (Lexer.getColEnd token)) ++ "\n"

-- retreive file/lineno/colno information as a 
-- nicely formatted string of a token
getErrInfo :: Expression -> String
getErrInfo (Id token) = Lexer.getErrHdr token
getErrInfo (Nat token) = Lexer.getErrHdr token
getErrInfo (Let token _ _ last) = 
   (Lexer.getErrHdr token) ++ " - " ++ (getErrInfo last)
getErrInfo (Lamda token _ _ last) = 
   (Lexer.getErrHdr token)  ++ " - " ++ (getErrInfo last)
getErrInfo (Unary token last) = 
   (Lexer.getErrHdr token) ++ " - " ++ (getErrInfo last)
getErrInfo (Binary _ exprl last) = 
   (getErrInfo exprl) ++ " - " ++ (getErrInfo last)
getErrInfo (Application expr last) = 
   (getErrInfo expr) ++ " - " ++ (getErrInfo last)
getErrInfo (Complex token _ last) = 
   (Lexer.getErrHdr token) ++ " - " ++ (Lexer.getErrHdr last)
getErrInfo _ = "Unknown location"

-- trivial code printing functions
getStrExpr :: Expression -> String
getStrExpr (Id token) = Lexer.getLexeme token
getStrExpr (Nat token) = Lexer.getLexeme token
getStrExpr (Let _ dst _ src) = 
   "Let " ++ (getStrExpr dst) ++ " = " ++(getStrExpr src)
getStrExpr (Lamda _ param _ body) =
   "\\" ++ (getStrExpr param) ++ "." ++ (getStrExpr body)
getStrExpr (Unary op expr) =
   Lexer.getLexeme op ++ (getStrExpr expr)
getStrExpr (Binary op exprl exprr) =
   (getStrExpr exprl) ++ " " ++ (Lexer.getLexeme op) ++ 
   " " ++ (getStrExpr exprr)
getStrExpr (Application exprl exprr) =
   (getStrExpr exprl) ++ " " ++ (getStrExpr exprr) 
getStrExpr (Complex _ exprl _ ) = ""
getStrExpr _ = ""

-- simple string generation for error handling
mkErrStr :: String -> Expression -> String
mkErrStr str expr = 
   "----------------------\n" ++
   "Syntax Error: " ++ str ++ "\n" ++
   (getErrInfo expr) ++ "\n" ++
   "\t" ++ (getStrExpr expr) ++ "\n" ++
   "----------------------\n"

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
parseId :: [Lexer.Token] -> (Status, [Lexer.Token], Expression, String )
parseId (h:tl) = (Success, tl, Id h, "")
-- handle the case where not enough tokens are present
parseId [] = (Failure, [], ErrExpr, "Bad id parse\n")

-- parse a NatTok into a Nat
parseNat :: [Lexer.Token] -> (Status, [Lexer.Token], Expression, String )
parseNat (h:tl) = (Success, tl, Nat h, "")
-- handle the case where not enough tokens are present
parseNat [] = (Failure, [], ErrExpr, "Bad nat parse\n")

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
parsePrimary :: [Lexer.Token] -> ( Status, [Lexer.Token], Expression, String )
parsePrimary (Lexer.Tok id x:tl) = 
   let tokens = (Lexer.Tok id x:tl)
   in  case id of
      Lexer.IdTok -> parseId tokens
      Lexer.NatTok -> parseNat tokens
--      Lexer.LParenTok -> parseCompound tokens
--         let (remainder, exprs, msg) = parseComplex (drop 1 tokens)
--            in if Lexer.isToken (head remainder) Lexer.RParen
--                  then ( drop 1 remainder, Complex id exprs (head remainder))
--                  else ( remainder, ErrExpr, "" )
      _ -> ( Failure, [], ErrExpr, "Unknown primary\n" )
-- handle the case where not enough tokens are present
parsePrimary [] = ( Failure, [], ErrExpr, "Bad primary\n" )

-- parse an application
parseApp :: [Lexer.Token] -> ( Status, [Lexer.Token], Expression, String )
parseApp (h:tl) =
   if status == Success
      then if status' == Success
            -- success for proper application
            then ( Success, remainder', Application primary app, "" )
            -- success for just a primary
            else ( Success, remainder, Application primary ENothing, "" )
      -- failure to parse
      else ( Failure, [], primary, msg )
   -- by greedy and assume its a proper application
   where ( status, remainder, primary, msg ) = parsePrimary (h:tl)
         ( status', remainder', app, _ ) = parseApp remainder
-- handle the case where not enough tokens are present
parseApp [] = ( Failure, [], ErrExpr, "Bad app\n" )

-- parse a factor
parseFactor :: [Lexer.Token] -> ( Status, [Lexer.Token], Expression, String )
parseFactor (h:tl) =
   case status of
      -- partial success
      Success -> 
         -- validat the next token is a factor operator
         if ( isHeadTok remainder Lexer.MultTok ) ||
               ( isHeadTok remainder Lexer.DivTok )
            -- partial success  
            then let (status', remainder', factor, msg') = 
                        parseFactor (drop 1 remainder)
                  in case status' of
                     -- successful parse
                     Success -> ( Success, remainder', 
                                 Binary (head remainder) app factor, "")
                     -- justpropagate the error
                     Failure -> ( Failure, [], factor, msg' )
            else ( Success, remainder, app, "" )
      -- just propagate the failure message
      Failure -> ( Failure, [], app, msg )
   where ( status, remainder, app, msg)  = parseApp (h:tl)
-- handle the case where not enough tokens are present
parseFactor [] = ( Failure, [], ErrExpr, "Bad factor\n" )

-- parse a term
parseTerm :: [Lexer.Token] -> ( Status, [Lexer.Token], Expression, String )
parseTerm (h:tl) = 
   case status of
      -- partial success
      Success -> 
         -- validat the next token is a term operator
         if ( isHeadTok remainder Lexer.PlusTok ) ||
               ( isHeadTok remainder Lexer.MinusTok )
               -- partial success
               then let (status', remainder', term, msg') = 
                           parseTerm (drop 1 remainder)
                  in case status' of
                     -- successful parse
                     Success -> ( Success, remainder', 
                                 ( Binary (head remainder) factor term), "")
                     -- just propagate the error
                     Failure -> ( Failure, [], term, msg' )
               -- failure on operator token
               else ( Success, remainder, factor, "" )
      -- just propagate the failure message
      Failure -> ( Failure, [], factor, msg )
   where ( status, remainder, factor, msg)  = parseFactor (h:tl)
-- handle the case where not enough tokens are present
parseTerm [] = ( Failure, [], ErrExpr, "Bad term\n" )

-- attempt to parse a (LetTok:tl) into a Let
parseLet :: [Lexer.Token] -> ( Status, [Lexer.Token], Expression, String )
parseLet (lt:id:eq:tl) =
   let tokens = (lt:id:eq:tl) 
   in if ( Lexer.isToken id IdTok ) && ( Lexer.isToken eq EqTok )
      -- partial success
      then let ( status, remainder, expr, msg ) = parseExpr (drop 3 tokens)
            in case status of
               Success -> ( Success, remainder, Let lt (Id id) eq expr, msg )
               -- just propagate the failure message
               Failure -> ( Failure, [], expr, msg )
      -- failure on id and/or assignment token
      else ( Failure, [], ErrExpr, "Bad let, token mismatch\n" ++
               (Lexer.getLexeme id) ++ (Lexer.getLexeme eq) ++ "\n" )
-- handle the case where not enough tokens are present
parseLet x = ( Failure, [], ErrExpr, "Bad let\n" )

-- attempt to parse a (LamdaTok:IdTok:DotTok:tl) into a Lamda
parseLamda :: [Lexer.Token] -> ( Status, [Lexer.Token], Expression, String )
parseLamda (lm:id:dt:tl) =
   let tokens = (lm:id:dt:tl)
   in if ( Lexer.isToken id IdTok ) && ( Lexer.isToken dt DotTok )
      -- partial success
      then let ( status, remainder, expr, msg ) = parseExpr (drop 3 tokens)
            in case status of
               -- complete success
               Success -> ( Success, remainder, Lamda lm (Id id) dt expr, msg )
               -- just propagate the failure message
               Failure -> ( Failure, [], expr, msg )
      -- failure on id and/or dot token
      else ( Failure, [], ErrExpr, "Bad lamda\n" )
-- handle the case where not enough tokens are present
parseLamda x = ( Failure, [], ErrExpr, "Bad lamda\n" )

-- break out the recognizable expression categories
parseExpr :: [Lexer.Token] -> (Status, [Lexer.Token], Expression, String)
parseExpr (Lexer.Tok id x:tl) =
   let tokens = (Lexer.Tok id x:tl)
   -- extract the type and pass the parse along
   in case id of
      LetTok      -> parseLet tokens
      LamdaTok    -> parseLamda tokens
      _           -> parseTerm tokens
-- return an error if someone tries to parse on a empty list
parseExpr [] = ( Failure, [], ErrExpr, "No more tokens" )

-- parse a single statement
parseStmt :: [Lexer.Token] -> ( Status, [Lexer.Token], Statement, String )
parseStmt (h:tl) =
   case status of
      -- if success from below then finish this parse
      Success -> if isHeadTok remainder Lexer.SemiTok
                  -- successful statement parse
                  then ( Success, drop 1 remainder, Stmt expr, "")
                  -- failed statment parse
                  else ( Failure, [], ErrStmt,
                     mkErrStr "statement missing semicolon" expr )
      -- just propagate if the error is from below
      Failure -> ( Failure, [], ErrStmt, msg )
   where ( status, remainder, expr, msg ) = parseExpr (h:tl)

-- simple statement collector
parseStmts :: [Lexer.Token] -> ([Statement], String)
parseStmts (h:tl) = 
   case status of 
      -- if we parsed one statement successfully proceed
      Success -> let (stmts, msgs) = parseStmts remainder
                  in ( (stmt:stmts), msg++msgs )
      -- otherwise terminate now
      Failure -> ( [], msg )
   -- attempt a statement parse
   where ( status, remainder, stmt, msg ) = parseStmt (h:tl)
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
