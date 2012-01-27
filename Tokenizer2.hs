module Tokenizer2
( tokenizeBuff
, tokenize
, TokId(..)
, Lexeme(..)
, Token(..)
, getTokenSymbol
, getTokenType
) where

import Data.Char as Char
import ListAux



-- Types of tokens
data TokId = LetTok | EqTok | LambdaTok | DotTok | NatTok | IdTok | SemiTok
           | LParenTok | RParenTok | PlusTok | MinusTok | MultTok | DivTok
           | UnknownTok deriving (Show,Eq)

-- structure: string, line #, column #, filename
data Lexeme = Lex String Int Int String deriving (Show,Eq)

-- structure: type, lexeme
data Token = Tok TokId Lexeme deriving (Show,Eq)

getTokenSymbol :: Token -> String
getTokenSymbol (Tok _(Lex s _ _ _)) = s

-- Shorthand to identify a Token by TokenType
getTokenType :: Token -> TokId
getTokenType (Tok t _) = t

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
    (t ++ [Tok IdTok (Lex (take n s) l c f)])
  where n = prefixPredLength Char.isAlphaNum s

pullNatural :: String ->Int -> Int -> String -> [Token] -> [Token]
pullNatural f l c s t =
  tokenize_impl
    f
    l
    (c + n)
    (drop n s)
    (t ++ [Tok NatTok (Lex (take n s) l c f)])
  where n = prefixPredLength Char.isDigit s

pullLet :: String -> Int -> Int -> String -> [Token] -> [Token]
pullLet f l c s t =
  tokenize_impl
    f
    l
    (c + 3)
    (drop 3 s)
    (t ++ [Tok EqTok (Lex "let" l c f)])

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
  | h == '\\' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok LambdaTok (Lex "\\" l c f)])
  | h == '.' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok DotTok (Lex "." l c f)])
  | h == '+' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok PlusTok (Lex "+" l c f)])
  | h == '*' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok MultTok (Lex "*" l c f)])
  | h == '-' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok MinusTok (Lex "-" l c f)])
  | h == '/' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok DivTok (Lex "/" l c f)])
  | h == '(' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok LParenTok (Lex "(" l c f)])
  | h == ')' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok RParenTok (Lex ")" l c f)])
  | h == ';' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok SemiTok (Lex ";" l c f)])
  | h == '=' = tokenize_impl f l (c + 1) (tail s) (t ++ [Tok EqTok (Lex "=" l c f)])
  | isPrefixKeyword s = pullLet f l c s t
  | Char.isAlpha h = pullIdentifier f l c s t
  | Char.isDigit h = pullNatural f l c s t
  | otherwise = t
  where h = head s



-- tokenizeSource takes a sourcefile and yields a list of token lists. Each
-- token list represents a statement
tokenizeBuff :: String -> String -> [[Token]]
tokenizeBuff fn src =
  splitAfter isTerminalToken $    -- break tokens appart into statements
    foldr (++) [] $                     -- Append all tokens together
    map (\(n,s) -> tokenize fn n s) $    -- Transforms each indexed src line to tokens
      zip [1..] (lines src) -- Returns an indexed list (line, str)

isTerminalToken :: Token -> Bool
isTerminalToken (Tok SemiTok _) = True
isTerminalToken _ = False


