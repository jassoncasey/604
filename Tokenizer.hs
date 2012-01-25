module Tokenizer
( tokenize
, Token_info(..)
, Token(..),
) where

import Data.Char as Char

-- Token_location defines where a token appears in source
--   String : filename
--   String : symbol
--   Int    : line
--   Int    : column
--   Int    : length of symbol (number chars)
data Token_info = Token_info String String Int Int Int deriving (Show)

data Token = Token_natural Int Token_info
           | Token_identifier Token_info
           | Token_operator Token_info
           | Token_left_parenth Token_info
           | Token_right_parenth Token_info
           | Token_lambda Token_info
           | Token_let Token_info
           | Token_dot Token_info
           | Token_define Token_info
           | Token_terminal Token_info deriving (Show)

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
tokenize filename line s = tokenize_line_impl filename line 1 s []



-- the pull functions lift the syntax
-- Move some of these off to detail
prefixPredLength :: (Char -> Bool) -> String -> Int
prefixPredLength _ [] = 0
prefixPredLength  pred (x:xs)
  | pred x = 1 + (prefixPredLength pred xs)
  | otherwise = 0

pullIdentifier :: String -> Int -> Int -> String -> [Token] -> [Token]
pullIdentifier f l c s t =
  tokenize_line_impl
    f
    l
    (c + n)
    (drop n s)
    (t ++ [Token_identifier (Token_info f (take n s) l c n)])
  where n = prefixPredLength Char.isAlphaNum s

pullNatural :: String ->Int -> Int -> String -> [Token] -> [Token]
pullNatural f l c s t =
  tokenize_line_impl
    f
    l
    (c + n)
    (drop n s)
    (t ++ [Token_natural (read (take n s) :: Int) (Token_info f (take n s) l c n)])
  where n = prefixPredLength Char.isDigit s

pullLet :: String -> Int -> Int -> String -> [Token] -> [Token]
pullLet f l c s t =
  tokenize_line_impl
    f
    l
    (c + 3)
    (drop 3 s)
    (t ++ [Token_let (Token_info f "let" l c 3)])

-- tokenize_lin_impl - tokenizes the string passed to it
--   f : filename
--   l : line number
--   c : column number
--   s : unparsed part of the string
--   t : list of tokenized stuff
tokenize_line_impl :: String ->Int -> Int -> String -> [Token] -> [Token]
tokenize_line_impl f l c s t
  | s == [] = t
  | h == ' ' || h == '\n' || h == '\t' = tokenize_line_impl f l (c + 1) (tail s) t
  | h == '\\' = tokenize_line_impl f l (c + 1) (tail s) (t ++ [Token_lambda (Token_info f "\\" l c 1)])
  | h == '.' = tokenize_line_impl f l (c + 1) (tail s) (t ++ [Token_dot (Token_info f "." l c 1)])
  | h == '+' = tokenize_line_impl f l (c + 1) (tail s) (t ++ [Token_operator (Token_info f "+" l c 1)])
  | h == '*' = tokenize_line_impl f l (c + 1) (tail s) (t ++ [Token_operator (Token_info f "*" l c 1)])
  | h == '(' = tokenize_line_impl f l (c + 1) (tail s) (t ++ [Token_left_parenth (Token_info f "(" l c 1)])
  | h == ')' = tokenize_line_impl f l (c + 1) (tail s) (t ++ [Token_right_parenth (Token_info f ")" l c 1)])
  | h == ';' = tokenize_line_impl f l (c + 1) (tail s) (t ++ [Token_terminal (Token_info f ";" l c 1)])
  | h == '=' = tokenize_line_impl f l (c + 1) (tail s) (t ++ [Token_define (Token_info f "=" l c 1)])
  | isPrefixKeyword s = pullLet f l c s t
  | Char.isAlpha h = pullIdentifier f l c s t
  | Char.isDigit h = pullNatural f l c s t
  where h = head s