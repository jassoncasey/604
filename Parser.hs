module Parser
( Statement
, Program
, parseNextStatement
, parseExpr
, parseLet
, findClosing
, innerTokens
) where

import Data.Maybe as Maybe

import Lexer


data Expr = Id Token | Nat Token | Let Expr Expr | Lambda Expr Expr
  | BinOp Token Expr Expr | Appl Expr Expr | Complex Expr Expr | NullExpr | ErrorExpr String deriving (Show)
type Statement = Expr
data Program = Program [Statement] deriving (Show)


-- TokenError Print
errorTokenPrint :: Token -> String
errorTokenPrint (Tok _ (Lex sym l c fn)) =
  "'" ++ sym ++ "' in file '" ++ fn ++ "' line " ++ (show l) ++ " col " ++ (show c) ++ "."

-- Get tokens inside parantheses
innerTokens :: [Token] -> Maybe (
innerTokens ts
  | Maybe.isNothing n = Nothing
  | otherwise = let n' = Maybe.fromJust n in Just (take (n'-1) $ tail ts)
  where n = findClosing ts 0 0

findClosing :: [Token] -> Int -> Int -> Maybe Int
findClosing [] _ _ = Nothing
findClosing (x:xs) i n
 | getTokenType x == LParenTok = findClosing xs (i+1) (n+1)
 | getTokenType x == RParenTok && n == 1 = Just i
 | getTokenType x == RParenTok && n /= 1 = findClosing xs (i+1) (n-1)
 | otherwise = findClosing xs (i+1) n

-- This function exits parsing of a statement
-- So it needs to jump to the next statement and report a string instead of an
-- expression
parseError :: Bool
parseError = False

--validStatement :: Statement -> Bool
--validStatement Right _ = True
--validStatement Left _ = False

-- Parses next statement in the program
parseNextStatement :: [Token] -> (Statement,[Token])
parseNextStatement [] = (NullExpr,[])
parseNextStatement tkns
  | isToken (head tkns) SemiTok = (NullExpr, dropWhile (isSemiTok) tkns)
  -- The next token statement
  | otherwise = (parseExpr nextTokenStatement, tail t)
  where (nextTokenStatement, t) = break (isSemiTok) tkns


-- Parses an expression
parseExpr :: [Token] -> Expr
parseExpr tokens =
  case id of
    LetTok    -> parseLet tokens
    LambdaTok -> parseLambda tokens
    NatTok    -> parseTerm tokens
    IdTok     -> parseTerm tokens
    LParenTok -> parseTerm tokens
    _         -> ErrorExpr ("Expression cannot start with " ++ (errorTokenPrint $ head tokens))
  where id = getTokenType $ head tokens


parseLet :: [Token] -> Expr
parseLet (t1:t2:t3:ts)
  | not $ isToken t2 IdTok = ErrorExpr ("Expected identifier, got " ++ errorTokenPrint t2)
  | not $ isToken t3 EqTok = ErrorExpr ("Expected '=' got " ++ errorTokenPrint t3)
  | null ts = ErrorExpr "Missing rest of let statement in file W line Y column Z"
  | otherwise = Let (Id t2) (parseExpr ts)
parseLet (t1:_) = ErrorExpr "Malformed let expression in file file W line Y column Z"

parseLambda :: [Token] -> Expr
parseLambda (t1:t2:t3:ts)
  | not $ isToken t2 IdTok = ErrorExpr ("Expected identifier, got " ++ errorTokenPrint t2)
  | not $ isToken t3 DotTok = ErrorExpr ("Expected '=' got " ++ errorTokenPrint t3)
  | null ts = ErrorExpr "Missing rest of lambda statement in file W line Y column Z"
  | otherwise = Lambda (Id t2) (parseExpr ts)
parseLambda (t1:_) = ErrorExpr "Malformed lambda expression in file file W line Y column Z"


parseTerm :: [Token] -> Expr
parseTerm [t] = parseFactor [t]
parseTerm (t1:t2:ts) =
  | 
  |
  |
  case id1 of
    LParenTok -> 
    _ -> case id2 of
      PlusTok  -> BinOp t2 (parseFactor [t1]) (parseTerm ts)
      MinusTok -> BinOp t2 (parseFactor [t1]) (parseTerm ts)
      _        -> parseFactor $ t1:t2:ts
    where id2 = getTokenType t2
  where id1 = getTokenType t1
parseTerm _ = ErrorExpr "Internal parser error 001"

parseFactor :: [Token] -> Expr
parseFactor [t] = parseApp [t]
parseFactor (t1:t2:ts) =
  case id2 of
    MultTok -> BinOp t2 (parseApp [t1]) (parseFactor ts)
    DivTok  -> BinOp t2 (parseApp [t1]) (parseFactor ts)
    _       -> parseApp $ t1:t2:ts
  where id2 = getTokenType t2
parseFactor _ = ErrorExpr "Internal parser error 002"

parseApp :: [Token] -> Expr
parseApp [t] = parsePrimary [t]
parseApp (t:ts) =
  case id of
    LParenTok -> parsePrimary (t:ts)
    _         -> Appl (parsePrimary [t]) (parseApp ts)
  where id = getTokenType t
parseApp _ = error "Internal parser error 003"

parsePrimary :: [Token] -> Expr
parsePrimary [t] =
  case id of
    NatTok    -> Nat t
    IdTok     -> Id t
    _      -> ErrorExpr ("Unrecognized token " ++ errorTokenPrint t)
  where id = getTokenType t
-- Only used for tokens
parsePrimary tkns =
  case id of
    LParenTok -> parseExpr $ inner
    _         -> ErrorExpr ("Unrecognized token " ++ errorTokenPrint (head tkns))
  where id = getTokenType $ head tkns