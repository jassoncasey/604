module PrettyPrint
( printTokenization
, printTokenList
) where

import Data.Maybe as Maybe
import Data.List( findIndex )

import Lexer

-- token symbol accessor
tokenSymbol :: Token -> String
tokenSymbol (Tok _ (Lex symbol _ _ _)) = symbol

printTokenLexeme :: Token -> String
printTokenLexeme (Tok t (Lex sym line col fn)) =
  (printTokenType t) ++ " '" ++ sym ++ "'" ++ " in file "
  ++ fn ++ " line " ++ (show line) ++ " column " ++ (show col)

-- String identification of token type
printTokenType :: TokId -> String
printTokenType s =
  case s of
    LetTok -> "Operator"
    EqTok -> "Operator"
    LambdaTok -> "Operator"
    DotTok -> "Operator"
    NatTok -> "Literal"
    IdTok -> "Identifier"
    SemiTok -> "Semi-colon"
    LParenTok -> "Left Parenthesis"
    RParenTok -> "Right Parenthesis"
    PlusTok -> "Operator"
    MinusTok -> "Operator"
    MultTok -> "Operator"
    DivTok -> "Operator "

-- Formatting detail, adds a space to the symbel held by token
symNeedsSpace :: Token -> Token -> Bool
symNeedsSpace (Tok t1 (Lex _ _ _ _)) (Tok t2 (Lex _ _ _ _)) =
  if elem t1 [LetTok,EqTok,DotTok,PlusTok,MinusTok,MultTok,DivTok]
  then True
  else if elem t1 [LambdaTok,SemiTok,LParenTok]
  then False
  else if elem t2 [EqTok,IdTok,NatTok,PlusTok,MinusTok,MultTok,DivTok]
  then True
  else False

symAddSpace :: Token -> Token
symAddSpace (Tok a (Lex sym b c d)) = (Tok a (Lex (sym ++ " ") b c d))

spaceTknSym :: [Token] -> [Token]
spaceTknSym [] = []
spaceTknSym [t] = [t]
spaceTknSym (t1:t2:ts)
  | symNeedsSpace t1 t2 = (symAddSpace t1) : spaceTknSym (t2:ts)
  | otherwise = t1 : spaceTknSym (t2:ts)
      

-- Appends padding string to each string in the list
addPadMap :: String -> [String] -> [String]
addPadMap padding ts = map (\x -> padding ++ x) ts

-- Prints all of the tokens in a token list
printTokenList :: [Token] -> String
printTokenList ts =
  foldr (++) "" (map (++"\n") (addPadMap "    " (map printTokenLexeme ts)))

-- Print the list of tokens as it would appear in source code
printTokenListAsStmt :: [Token] -> String
printTokenListAsStmt ts =
  let
    tkn_sym = map tokenSymbol $ spaceTknSym ts
  in foldr (++) ""  tkn_sym

-- Tries to parse the code. If it can't, return false
isParseSuccess :: [Token] -> Bool
isParseSuccess _ = True

-- Prints a statement and it's token list if a successful parse
printIfParsed :: [Token] -> String
printIfParsed tkns
  | isParseSuccess tkns = "\n  " ++ (printTokenListAsStmt tkns)
      ++ (printTokenList tkns)
  | otherwise = "\n  " ++ "Parse error: Statement starting at line "
      ++ (show $ l $ head tkns) ++ "is not valid.\n"
  where l = (\(Tok _ (Lex _ line _ _)) -> line)

-- Basically show for parsing
printTokenization :: String -> String -> String
printTokenization fn src =
  let
    tk_src = splitAfter isSemiTok $ tokenizeBuff fn src
  in fn ++ (foldr (++) "" $
    map (\t -> "\n  " ++ (printTokenListAsStmt t) ++ (printTokenList t)) tk_src)


breakAfter :: (a -> Bool) -> [a] -> ([a],[a])
breakAfter _ [] = ([],[])
breakAfter p l
  | Maybe.isJust n = let n' = fromJust n + 1 in (take n' l, drop (n'-1) l)
  | otherwise = (l,[])
  where n = findIndex p l


-- splitAfter splits a list when it encounters an element that does not satisfy
-- the predicate and includes the predicate with the preceding elements
-- Example splitAfter (< 3) [1,2,3,1,2,4,2,4] = [[1,2,3],[1,2,4],[2,4]]
splitAfter :: (a -> Bool) -> [a] -> [[a]]
splitAfter p l = filter (not . null) $ splitAfterImpl p l

splitAfterImpl :: (a -> Bool) -> [a] -> [[a]]
splitAfterImpl p s =
  let (l, s') = breakAfter p s
  in l : case s' of
    [] -> []
    (_: s'') -> splitAfterImpl p s''

isSemiTok :: Token -> Bool
isSemiTok (Tok SemiTok _) = True
isSemiTok _ = False
