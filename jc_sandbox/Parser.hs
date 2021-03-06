module Parser (
   parse,
   getStrProg,
   Expression(..),
   Statement(..),
   Program(..)
) where

import Lexer as Lexer

-- Simple Programming Language (SPL) parse representation
data Expression = Id Lexer.Token
                  | Nat Lexer.Token
                  | Let Lexer.Token Expression Lexer.Token Expression
                  | Lamda Lexer.Token Expression Lexer.Token Expression
                  | Unary Lexer.Token Expression
                  | Binary Lexer.Token Expression Expression
                  | Application Expression Expression
                  | Compound Lexer.Token [Expression] Lexer.Token
                  | ErrExpr
                  deriving (Show,Eq)
data Statement = Stmt Expression 
               | ErrStmt 
               deriving (Show,Eq)
data Program   = Prog [Statement]
               | ErrPrg
               deriving (Show,Eq)

data Status = Success | Failure deriving (Show,Eq)

getSemiStr (h:tl) = 
   if (length result) > 0
      then (getStrExpr h) ++ ";" ++ result
      else getStrExpr h
   where result = getSemiStr tl
getSemiStr [] = ""

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
getStrExpr (Compound _ exprs _ ) = "(" ++ (getSemiStr exprs) ++ ")"
getStrExpr _ = "Unknown expression"

getStrStmt :: Statement -> String
getStrStmt (Stmt expr) = (getStrExpr expr) ++ ";"

getStrStmts :: [Statement] -> String
getStrStmts (h:tl) = (getStrStmt h) ++ "\n" ++ (getStrStmts tl)
getStrStmts [] = ""

getStrProg :: Program -> String
getStrProg (Prog stmts) = getStrStmts stmts

-- simple string generation for error handling
mkErrStr :: String -> Lexer.Token -> String
mkErrStr str token = 
   "----------------------\n" ++
   "Syntax Error: " ++ str ++ "\n" ++
   "Line: " ++ (Lexer.getLineNo token) ++ " " ++
   "Column: " ++ (Lexer.getColStart token) ++ "\n" ++
   "----------------------\n"

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
parseId [] = (Failure, [], ErrExpr,
               mkErrStr "Id requires more tokens" Lexer.Empty)

-- parse a NatTok into a Nat
parseNat :: [Lexer.Token] -> (Status, [Lexer.Token], Expression, String )
parseNat (h:tl) = (Success, tl, Nat h, "")
-- handle the case where not enough tokens are present
parseNat [] = (Failure, [], ErrExpr, 
               mkErrStr "Nat requires more tokens" Lexer.Empty )

-- parse the inside of a compound statment
parseInnerCmp :: [Lexer.Token] -> [Expression] -> Lexer.Token -> ( Status, [Lexer.Token], Expression, String )
parseInnerCmp (h:tl) exprs hd =
   case getTokId h of
      Lexer.RParenTok -> ( Success, tl, Compound hd exprs h, "" )
      Lexer.SemiTok -> 
         let ( status, remainder, expr, msg ) = parseExpr tl
         in case status of
            Success -> parseInnerCmp remainder (exprs ++ [expr]) hd
            Failure -> ( Failure, [], ErrExpr, "" )
      _ -> ( Failure, [], ErrExpr, "" )
parseInnerCmp [] _ _ = ( Failure, [], ErrExpr,
                     mkErrStr "Inner compound requires more tokens" Lexer.Empty)

-- parse the outer portion of the compound expression
parseCompound :: [Lexer.Token] -> ( Status, [Lexer.Token], Expression, String )
parseCompound (h:tl) =
   case status of
      Success -> parseInnerCmp remainder [expr] h
      -- failed to parse expression
      Failure -> ( Failure, [], ErrExpr, msg )
   -- don't worry about LParen
   where ( status, remainder, expr, msg ) = parseExpr tl
-- handle the case where not enough tokens are present
parseCompound [] = ( Failure, [], ErrExpr, 
                     mkErrStr "Compound requires more tokens" Lexer.Empty)

-- attempt to parse the primary terms and non-term
parsePrimary :: [Lexer.Token] -> ( Status, [Lexer.Token], Expression, String )
parsePrimary (Lexer.Tok id x:tl) = 
   let tokens = (Lexer.Tok id x:tl)
   in  case id of
      Lexer.IdTok -> parseId tokens
      Lexer.NatTok -> parseNat tokens
      Lexer.LParenTok -> parseCompound tokens
      _ -> ( Failure, [], ErrExpr, mkErrStr "Unknown primary" (Lexer.Tok id x))
-- handle the case where not enough tokens are present
parsePrimary [] = ( Failure, [], ErrExpr, 
                     mkErrStr "Primary requires more tokens" Lexer.Empty )

-- parse an application
parseApp :: [Lexer.Token] -> ( Status, [Lexer.Token], Expression, String )
parseApp (h:tl) =
   if status == Success
      then if status' == Success
            -- success for proper application
            then ( Success, remainder', Application primary app, "" )
            -- success for just a primary
            else ( Success, remainder, primary, "" )
      -- failure to parse
      else ( Failure, [], primary, msg )
   -- by greedy and assume its a proper application
   where ( status, remainder, primary, msg ) = parsePrimary (h:tl)
         ( status', remainder', app, _ ) = parseApp remainder
-- handle the case where not enough tokens are present
parseApp [] = ( Failure, [], ErrExpr, 
               mkErrStr "Application requires more tokens" Lexer.Empty )

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
parseFactor [] = ( Failure, [], ErrExpr, 
                  mkErrStr "Factor requires more tokens" Lexer.Empty )

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
parseTerm [] = ( Failure, [], ErrExpr, 
                  mkErrStr "Term requires more tokens" Lexer.Empty )

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
      else ( Failure, [], ErrExpr, mkErrStr "Let has invalid binding" lt )
-- handle the case where not enough tokens are present
parseLet x = ( Failure, [], ErrExpr, 
               mkErrStr "Let requires more tokens" Lexer.Empty )

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
      else ( Failure, [], ErrExpr, 
         mkErrStr "Lamda incorrectly specified" lm )
-- handle the case where not enough tokens are present
parseLamda x = ( Failure, [], ErrExpr, 
                  mkErrStr "Lamda requires more tokens" Lexer.Empty )

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
parseExpr [] = ( Failure, [], ErrExpr, 
                  mkErrStr "Expression requires more tokens" Lexer.Empty )

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
                     mkErrStr "Statement missing semicolon" h )
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
       (prog, msg) = parseImp tokens
   in if (length msg) > 0
      then (prog, msg)
      else (prog, getStrProg prog)
