module Tokenizer
( tokenizeSource
, tokenize
, TokenType(..)
, Token(..)
, getTokenSymbol
, getTokenType
) where

import Data.Char as Char
import ListAux



-- Types of tokens
data TokenType = TokLit | TokId | TokBinOp | TokLambda | TokDot
  | TokLet | TokDef | TokSemi | TokParL | TokParR deriving (Show)

-- Data type represents a token as: type filename symbol line column
data Token = Token TokenType String String Int Int deriving (Show)

getTokenSymbol :: Token -> String
getTokenSymbol (Token _ _ s _ _) = s

-- Shorthand to identify a Token by TokenType
getTokenType :: Token -> TokenType
getTokenType (Token t _ _ _ _) = t

{- Checks for keywords and pulls keywords.
   May be more useful as the set of keywords increases.
   Only checks for let right now -}
isPrefixKeyword :: String -> Bool
isPrefixKeyword s
  | (take 3 s) == "let" = True
  | otherwise = False

-- Tokenizes a line
-- FIXME Does multipasss compilation! put these anywhere!
tokenize :: String -> Int -> String -> [Token]
tokenize filename line s = tokenize_impl filename line 1 s []



-- the pull functions lift the syntax
-- Move some of these off to detail
prefixPredLength :: (Char -> Bool) -> String -> Int
prefixPredLength _ [] = 0
prefixPredLength  pred (x:xs)
  | pred x = 1 + (prefixPredLength pred xs)
  | otherwise = 0

pullIdentifier :: String -> Int -> Int -> String -> [Token] -> [Token]
pullIdentifier f l c s t =
  tokenize_impl
    f
    l
    (c + n)
    (drop n s)
    (t ++ [Token TokId f (take n s) l c])
  where n = prefixPredLength Char.isAlphaNum s

pullNatural :: String ->Int -> Int -> String -> [Token] -> [Token]
pullNatural f l c s t =
  tokenize_impl
    f
    l
    (c + n)
    (drop n s)
    (t ++ [Token TokLit f (take n s) l c])
  where n = prefixPredLength Char.isDigit s

pullLet :: String -> Int -> Int -> String -> [Token] -> [Token]
pullLet f l c s t =
  tokenize_impl
    f
    l
    (c + 3)
    (drop 3 s)
    (t ++ [Token TokLet f "let" l c])

-- tokenize_lin_impl - tokenizes the string passed to it
--   f : filename
--   l : line number
--   c : column number
--   s : unparsed part of the string
--   t : list of tokenized stuff
tokenize_impl :: String -> Int -> Int -> String -> [Token] -> [Token]
tokenize_impl f l c s t
  | s == [] = t
  | h == ' ' || h == '\n' || h == '\t' = tokenize_impl f l (c + 1) (tail s) t
  | h == '\\' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokLambda f "\\" l c])
  | h == '.' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokDot f "." l c])
  | h == '+' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokBinOp f "+" l c])
  | h == '*' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokBinOp f "*" l c])
  | h == '-' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokBinOp f "-" l c])
  | h == '/' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokBinOp f "/" l c])
  | h == '(' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokParL f "(" l c])
  | h == ')' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokParR f ")" l c])
  | h == ';' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokSemi f ";" l c])
  | h == '=' = tokenize_impl f l (c + 1) (tail s) (t ++ [Token TokDef f "=" l c])
  | isPrefixKeyword s = pullLet f l c s t
  | Char.isAlpha h = pullIdentifier f l c s t
  | Char.isDigit h = pullNatural f l c s t
  | otherwise = t
  where h = head s



-- tokenizeSource takes a sourcefile and yields a list of token lists. Each
-- token list represents a statement
tokenizeSource :: String -> String -> [[Token]]
tokenizeSource fn src =
  splitAfter isTerminalToken $    -- break tokens appart into statements
    foldr (++) [] $                     -- Append all tokens together
    map (\(n,s) -> tokenize fn n s) $    -- Transforms each indexed src line to tokens
      zip [1..(length src)] (lines src) -- Returns an indexed list (line, str)



isTerminalToken :: Token -> Bool
isTerminalToken (Token TokSemi _ _ _ _) = True
isTerminalToken _ = False


