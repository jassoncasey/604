module Parser (
   parse,
) where

-- Import the intput type
import Lexer
-- Import the output type
import Ast

{-- The parser returns a programm, that program may
 -  be valid or invalid depending on proper construction.
 -  The parser is annotated with rules from the LL grammar
 -  for SPL. Helper functions are not annotated because
 -  they are not part of the grammar.
-}

-- parse a filename and string buffer to a representation and output msgs
parse :: String -> String -> Program
parse fname buf = 
   let tokens = Lexer.tokenizeBuff fname buf
   in parseImp tokens

-- main function of the parser
parseImp :: [Lexer.Token] -> Program
parseImp (h:tl) = parseProgram (h:tl)
parseImp [] = Prog []

{--Grammar--
   program : statements
-}

-- start the root of the parser
parseProgram :: [Lexer.Token] -> Program
parseProgram tokens = Prog stmts
   where stmts = parseStmts tokens

{--Grammar--
-  statements : statement statements
-             | Empty
-}

-- simple statement collector
parseStmts :: [Lexer.Token] -> [Statement]
parseStmts (h:tl) = 
      -- if we parsed one statement successfully proceed
      let stmts = parseStmts remainder in (stmt:stmts)
   -- attempt a statement parse
   where ( remainder, stmt ) = parseStmt (h:tl)
parseStmts [] = [] 

{--Grammar--
	statement : expression ';'
-}

-- parse a single statment
parseStmt :: [Lexer.Token] -> ( [Lexer.Token], Statement )
parseStmt (h:tl) =
   if isHeadToken remainder Lexer.SemiTok
      -- successful statement parse
      then ( drop 1 remainder, Stmt expr)
      -- failed statment parse
      else ( [], ErrStmt "Statement missing semicolon" )
  where ( remainder, expr ) = parseExpr (h:tl)

{--Grammar--
   expression  : 'let' Id '=' expression
               | '\' Id '.' expression
               | term
-}

-- break out the recognizable expression categories
parseExpr :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parseExpr (Lexer.Tok id x:tl) =
   let tokens = (Lexer.Tok id x:tl)
   -- extract the type and pass the parse along
   in case id of
      LetTok      -> parseLet tokens
      LambdaTok    -> parseLambda tokens
      _           -> parseTerm tokens
-- return an error if someone tries to parse on a empty list
parseExpr [] = ( [], ErrExpr "Expression requires more tokens" ) 

-- attempt to parse a (LambdaTok:IdTok:DotTok:tl) into a Lambda
parseLambda :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parseLambda (lm:id:dt:tl) =
   let tokens = (lm:id:dt:tl)
   in if ( Lexer.isToken id IdTok ) && ( Lexer.isToken dt DotTok )
      -- partial success
      then let ( remainder, expr ) = parseExpr (drop 3 tokens)
            in ( remainder, Lambda lm (Id id) dt expr )
      -- failure on id and/or dot token
      else ( [], ErrExpr "Lambda incorrectly specified" )
-- handle the case where not enough tokens are present
parseLambda x = ( [], ErrExpr "Lambda requires more tokens" )

-- attempt to parse a (LetTok:tl) into a Let
parseLet :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parseLet (lt:id:eq:tl) =
   let tokens = (lt:id:eq:tl) 
   in if ( Lexer.isToken id IdTok ) && ( Lexer.isToken eq EqTok )
      -- partial success
      then let ( remainder, expr ) = parseExpr (drop 3 tokens)
            in ( remainder, Let lt (Id id) eq expr )
      -- failure on id and/or assignment token
      else ( [], ErrExpr "Let has invalid binding" )
-- handle the case where not enough tokens are present
parseLet x = ( [], ErrExpr "Let requires more tokens" )

{--Grammar--
   term  : factor '+' term
         | factor '-' term
         | term
-}

-- parse a term
parseTerm :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parseTerm (h:tl) = 
   -- validat the next token is a term operator
   if ( isHeadToken remainder Lexer.PlusTok ) ||
         ( isHeadToken remainder Lexer.MinusTok )
      -- partial success
      then let ( remainder', term ) = parseTerm (drop 1 remainder)
            in ( remainder', ( Binary (head remainder) factor term) )
      -- failure on operator token
      else ( remainder, factor )
   where ( remainder, factor )  = parseFactor (h:tl)
-- handle the case where not enough tokens are present
parseTerm [] = ( [], ErrExpr "Term requires more tokens" )

{--Grammar
   factor   : application '*' factor
            | application '/' factor
            | application
-} 

-- parse a factor
parseFactor :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parseFactor (h:tl) =
   -- validat the next token is a factor operator
   if ( isHeadToken remainder Lexer.MultTok ) ||
         ( isHeadToken remainder Lexer.DivTok )
      -- partial success  
      then let ( remainder', factor ) = parseFactor (drop 1 remainder)
              in ( remainder', Binary (head remainder) app factor )
      else ( remainder, app )
   where ( remainder, app )  = parseApp (h:tl)
-- handle the case where not enough tokens are present
parseFactor [] = ( [], ErrExpr "Factor requires more tokens" )

{--Grammar
   application : primary application
               | primary
-}

-- parse an application
parseApp :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parseApp (h:tl) =
   -- call a helper to split an application from a lone primary
   appCheck primary app remainder' remainder
   -- by greedy and assume its a proper application
   where ( remainder, primary ) = parsePrimary (h:tl)
         ( remainder', app ) = parseApp remainder
-- handle the case where not enough tokens are present
parseApp [] = ( [], ErrExpr "Application requires more tokens" )

-- helper function to split a primary from an application
appCheck :: Expression -> Expression -> [Lexer.Token] -> 
            [Lexer.Token] -> ( [Lexer.Token], Expression )
appCheck prim (ErrExpr _) _ rem  = ( rem, prim )
appCheck prim app rem' _ = ( rem', Application prim app )

{--Grammar
   primary  : Identifier
            | Natural
            | '(' compound ')'
-}

-- attempt to parse the primary terms and non-term
parsePrimary :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parsePrimary (Lexer.Tok id x:tl) = 
   let tokens = (Lexer.Tok id x:tl)
   in  case id of
      Lexer.IdTok -> parseId tokens
      Lexer.NatTok -> parseNat tokens
      Lexer.LParenTok -> parseCompound tokens
      _ -> ( [], ErrExpr "Unknown primary" )
-- handle the case where not enough tokens are present
parsePrimary [] = ( [], ErrExpr "Primary requires more tokens" )

-- parse an IdTok into a Id
parseId :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parseId (h:tl) = ( tl, Id h )
-- handle the case where not enough tokens are present
parseId [] = ( [], ErrExpr "Id requires more tokens" )

-- parse a NatTok into a Nat
parseNat :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parseNat (h:tl) = ( tl, Nat h )
-- handle the case where not enough tokens are present
parseNat [] = ( [], ErrExpr "Nat requires more tokens" )

{--Grammar--
   compound : expression ';' compound
            : expression
-}

-- parse the outer portion of the compound expression
parseCompound :: [Lexer.Token] -> ( [Lexer.Token], Expression )
parseCompound (h:tl) =
   if isHeadToken tl SemiTok
      then parseInnerCmp remainder [(expr,head remainder)] h
      else parseInnerCmp remainder [(expr,EmptyTok)] h
   -- don't worry about LParen
   where ( remainder, expr ) = parseExpr tl
-- handle the case where not enough tokens are present
parseCompound [] = ( [], ErrExpr "Compound requires more tokens" )

-- parse the inside of a compound statment
parseInnerCmp :: [Lexer.Token] -> [Expression] -> Lexer.Token -> 
                  ( [Lexer.Token], Expression )
parseInnerCmp (Lexer.Tok id h:tl) exprs hd =
   case h of
      Lexer.RParenTok -> ( tl, Compound hd exprs h )
      Lexer.SemiTok -> 
         let ( remainder, expr ) = parseExpr tl
            in if isHeadToken remainder SemiTok 
               then parseInnerCmp remainder (exprs ++ [(expr,head remainder)]) hd
               else parseInnerCmp remainder (exprs ++ [(expr,EmptyTok)]) hd
      _ -> ( [], ErrExpr "Unknown compound terminator" )
parseInnerCmp [] _ _ = ( [], ErrExpr "Inner compound requires more tokens" )

