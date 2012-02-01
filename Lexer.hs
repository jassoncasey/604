module Lexer
( tokenizeBuff
, tokenize
, TokId(..)
, Lexeme(..)
, Token(..)
, isToken
, isHeadToken
) where

import Data.Char as Char
import Data.List as List

-- SPL Token types and data structure
-- types: let, =, \, ., <natural number>, <identifier>, ;, (, )
data TokId = LetTok | EqTok | LambdaTok | DotTok | NatTok | IdTok | SemiTok
           | LParenTok | RParenTok | PlusTok | MinusTok | MultTok | DivTok
           | UnknownTok String deriving (Show,Eq)

-- structure: lexeme, line #, column #, filename
data Lexeme = Lex String Int Int String deriving (Show,Eq)

-- structure: type, lexeme
data Token = Tok TokId Lexeme | ErrorTok String | EmptyTok deriving (Show,Eq)

-- simple token id predicates
isToken :: Token -> TokId -> Bool
isToken (Tok token _ ) match = token == match

-- peak at the head and validate its token id
isHeadToken :: [Lexer.Token] -> Lexer.TokId -> Bool
isHeadToken (Lexer.Tok value _:tl) id = value == id
isHeadToken tokens id = False

{- Get functions
  Functions to retrieve the contents of Tokens
-}
getFile :: Token -> String
getFile (Tok _ (Lex _ _ _ filename)) = filename
getLineNo :: Token -> String
getLineNo (Tok _ (Lex _ line _ _)) = show line
getColStart :: Token -> String
getColStart (Tok _ (Lex _ _ col _)) = show col
getColEnd :: Token -> String
getColEnd (Tok _ (Lex sym _ col _)) = show $ col + (length sym) - 1



{- tokenizeBuff
  Input:  filename = name of file whose contents is in buffer
  Output: token representation of the spl source file
-}
tokenizeBuff :: String -> String -> [Token]
tokenizeBuff fn src =
  -- concatenate  tokenized lines
  foldr (++) [] $
    -- tokenize each line of source
    map (\(lineNum, s) -> tokenize fn lineNum s) $
      -- number each line of source for error reporting reasons
      zip [1..] (lines src)

{- tokenize
  Input:  filename :: String = name of file that the source comes from
          lineNum :: Int = line number that the source code appears in file
          src :: String = source code
  Output: token representation of source code
-}
tokenize :: String -> Int -> String -> [Token]
tokenize fn lineNum src = tokenize_impl fn lineNum 1 src []
-- tokenizes a line: input = filename, lineNum, colNum, source, tokens
tokenize_impl :: String -> Int -> Int -> String -> [Token] -> [Token]
tokenize_impl f l c s t
  | s == [] = t
  | h == ' ' || h == '\n' || h == '\t' = tokenize_impl f l (c + 1) (tail s) t
  | isCharOp h = tokenize_impl f l (c+1) (tail s) (t ++ [Tok (getCharOp h) (Lex [h] l c f)])
  | isPrefixKeyword s = pullLet f l c s t
  | (Char.isAlpha) h || (h ==) '_' = pullIdentifier f l c s t
  | Char.isDigit h = pullNatural f l c s t
  | otherwise = [Tok (UnknownTok "Unrecognized symbol at") (Lex [h] l c f)]
  where h = head s

isPrefixKeyword :: String -> Bool
isPrefixKeyword s
  | List.isPrefixOf "let" s = True
  | otherwise = False

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
pullNatural f l c s t
  | trailingChar (drop n s) = [Tok
    (UnknownTok "Number may not have trailing characters.")
    (Lex (drop n $take (n+1) s) l c f)]
  | otherwise = tokenize_impl f l (c + n) (drop n s)
      (t ++ [Tok NatTok (Lex (take n s) l c f)])
  where n = prefixPredLength Char.isDigit s
trailingChar :: String -> Bool
trailingChar [] = False
trailingChar (c:cs) = (Char.isAlpha c) || c == '_'

pullLet :: String -> Int -> Int -> String -> [Token] -> [Token]
pullLet f l c s t =
  tokenize_impl
    f
    l
    (c + 3)
    (drop 3 s)
    (t ++ [Tok LetTok (Lex "let" l c f)])

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
