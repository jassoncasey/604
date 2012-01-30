module Parser
( Statement
, Program
, parseNextStatement
, parseStatements
, parseExpr
, parseLet
, findClosing
, breakCompound
) where

import Data.Maybe as Maybe

import Lexer


data Expr = Id Token | Nat Token | Let Expr Expr | Lambda Expr Expr
  | BinOp Token Expr Expr | Appl Expr Expr | NullExpr
  | ErrorExpr String deriving (Show)
type Statement = Expr
data Program = Program [Statement] deriving (Show)


-- common error string for a token
errorTokenPrint :: Token -> String
errorTokenPrint (Tok _ (Lex sym l c fn)) =
  "'" ++ sym ++ "' in file '" ++ fn ++ "' line " ++ (show l) ++ " col " ++ (show c) ++ "."

-- Get tokens inside parantheses
breakCompound :: [Token] -> Maybe ([Token],[Token])
breakCompound ts =
  case closingIndex of
    Nothing -> Nothing
    Just n -> let (inner, rest) = splitAt n ts in Just (tail inner, tail rest)
  where closingIndex = findClosingParen ts
findClosingParen :: [Token] -> Maybe Int
findClosingParen tokens = findClosing tokens 0 0
findClosing :: [Token] -> Int -> Int -> Maybe Int
findClosing [] _ _ = Nothing
findClosing (x:xs) i n
 | getTokenType x == LParenTok = findClosing xs (i+1) (n+1)
 | getTokenType x == RParenTok && n == 1 = Just i
 | getTokenType x == RParenTok && n /= 1 = findClosing xs (i+1) (n-1)
 | otherwise = findClosing xs (i+1) n


-- getParseError
-- Input:  Expr = an expression (output of parseExpr)
-- Output: Maybe String = First error that occurred during parsing is returned.
--                        If the expression contains no error, Nothing is returned
getParseError :: Expr -> Maybe String
getParseError (ErrorExpr err) = Just err
getParseError (Id _) = Nothing
getParseError (Nat _) = Nothing
getParseError (Let e1 e2) =
  case e of
    (Just a, _) -> Just a
    (Nothing, Just a) -> Just a
    (Nothing,Nothing) -> Nothing
  where e = (getParseError e1,getParseError e2)
getParseError (Lambda e1 e2) =
  case e of
    (Just a, _) -> Just a
    (Nothing, Just a) -> Just a
    (Nothing,Nothing) -> Nothing
  where e = (getParseError e1,getParseError e2)
getParseError (BinOp _ e1 e2) =
  case e of
    (Just a, _) -> Just a
    (Nothing, Just a) -> Just a
    (Nothing,Nothing) -> Nothing
  where e = (getParseError e1,getParseError e2)
getParseError (Appl e1 e2) =
  case e of
    (Just a, _) -> Just a
    (Nothing, Just a) -> Just a
    (Nothing,Nothing) -> Nothing
  where e = (getParseError e1,getParseError e2)
getParseError NullExpr = Nothing


parseStatements :: [Token] -> [Statement]
parseStatements tokens =
  case ts of
    [] -> [s]
    _  -> s : (parseStatements ts)
  where (s,ts) = parseNextStatement tokens

-- Parses next statement in the program
parseNextStatement :: [Token] -> (Statement,[Token])
parseNextStatement [] = (NullExpr,[])
parseNextStatement tkns
  | isToken (head tkns) SemiTok = (NullExpr, dropWhile (isSemiTok) tkns)
  | otherwise = case t of
    [] -> (parseExpr nextTokenStatement, [])
    _  -> (parseExpr nextTokenStatement, tail t)
  where (nextTokenStatement, t) = break (isSemiTok) tkns


-- Parses an expression. The result
parseExpr :: [Token] -> Expr
parseExpr [] = NullExpr
parseExpr tokens =
  case id of
    LetTok    -> parseLet tokens
    LambdaTok -> parseLambda tokens
    NatTok    -> parseTerm tokens
    IdTok     -> parseTerm tokens
    LParenTok -> parseTerm tokens
    FoldTok _ -> parseTerm tokens
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
  case id2 of
    PlusTok  -> BinOp t2 (parseFactor [t1]) (parseTerm ts)
    MinusTok -> BinOp t2 (parseFactor [t1]) (parseTerm ts)
    _        -> parseFactor $ t1:t2:ts
  where id2 = getTokenType t2
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
    NatTok       -> Nat t
    IdTok        -> Id t
    FoldTok _    -> parseExpr $ unwrapFoldToken t
    _      -> ErrorExpr ("Unrecognized token " ++ errorTokenPrint t)
  where id = getTokenType t
parsePrimary tkns =
  case id of
    LParenTok -> let inner = breakCompound tkns in case inner of
      Nothing -> ErrorExpr "Could not find closing brace in FILE line LINE"
      Just (tkns1, tkns2) -> parseExpr ((Tok (FoldTok tkns1) (Lex "?fold?" 0 0 "null")):tkns2)
    _           -> ErrorExpr ("Unrecognized token " ++ (errorTokenPrint $ head tkns))
  where id = getTokenType $ head tkns




