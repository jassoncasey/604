module Lexer
( tokenizeBuff
, tokenize
, TokId(..)
, Lexeme(..)
, Token(..)
, getTokenSymbol
, getTokenType
, prefixPredLength
, isSemiTok
) where

import Data.Char as Char
import Data.List as List
import ListAux



-- Types of tokens
data TokId = LetTok | EqTok | LambdaTok | DotTok | NatTok | IdTok | SemiTok
           | LParenTok | RParenTok | PlusTok | MinusTok | MultTok | DivTok
           | UnknownTok deriving (Show,Eq)

-- structure: string, line #, column #, filename
data Lexeme = Lex String Int Int String deriving (Show,Eq)

-- structure: type, lexeme
data Token = Tok TokId Lexeme deriving (Show,Eq)

-- extract the lexeme of a token
getLexeme :: Token -> String
getLexeme token = lexeme
   where (Lexer.Tok _ (Lex lexeme _ _ _)) = token

-- provide a string containing the relavent info of this token
getErrHdr :: Token -> String
getErrHdr token = 
   "Filename: " ++ fname ++ "\nLo:" ++ (show lineno) ++ 
   " Col:" ++  (show colno) ++ " : "
   where (Lexer.Tok _ (Lex lexeme lineno colno fname)) = token

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
  | List.isPrefixOf "let" s = True
  | otherwise = False

-- Tokenizes a line
tokenize :: String -> Int -> String -> [Token]
tokenize filename line s = tokenize_impl filename line 1 s []

-- the pull functions lift the syntax
-- Move some of these off to detail
prefixPredLength :: (Char -> Bool) -> String -> Int
prefixPredLength p s = length (takeWhile p s)

pullIdentifier :: String -> Int -> Int -> String -> [Token] -> [Token]
pullIdentifier f l c s t = let
  p = (\x -> Char.isAlphaNum x || x == '_')
  n = prefixPredLength p s
  in
  tokenize_impl
    f
    l
    (c + n)
    (drop n s)
    (t ++ [Tok IdTok (Lex (take n s) l c f)])

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

isCharOp :: Char -> Bool
isCharOp c = elem c "\\.+*-/();="
getCharOp :: Char -> TokId
getCharOp c =
  case c of
    '\\' -> LambdaTok
    '.' -> DotTok
    '+' -> PlusTok
    '*' -> MultTok
    '-' -> MinusTok
    '/' -> DivTok
    '(' -> LParenTok
    ')' -> RParenTok
    ';' -> SemiTok
    '=' -> EqTok


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
  | isCharOp h = tokenize_impl f l (c+1) (tail s) (t ++ [Tok (getCharOp h) (Lex [h] l c f)])
  | isPrefixKeyword s = pullLet f l c s t
  | (Char.isAlpha) h || (h ==) '_' = pullIdentifier f l c s t
  | Char.isDigit h = pullNatural f l c s t
  | otherwise = tokenize_impl f l (c+1) (tail s) (t ++ [Tok UnknownTok (Lex [h] l c f)])
  where h = head s



-- tokenizeSource takes a sourcefile and yields a list of token lists. Each
-- token list represents a statement
tokenizeBuff :: String -> String -> [[Token]]
tokenizeBuff fn src =
  splitAfter isSemiTok $    -- break tokens appart into statements
    foldr (++) [] $                     -- Append all tokens together
    map (\(n,s) -> tokenize fn n s) $    -- Transforms each indexed src line to tokens
      zip [1..] (lines src) -- Returns an indexed list (line, str)

isSemiTok :: Token -> Bool
isSemiTok (Tok SemiTok _) = True
isSemiTok _ = False


