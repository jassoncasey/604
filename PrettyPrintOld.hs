module PrettyPrint
( printTokenization
) where

import Tokenizer
import ListAux

printTokenInfo :: Token -> String
printTokenInfo (Token t fn sym line col) =
  (printTokenType t) ++ " '" ++ sym ++ "'" ++ " in file "
  ++ fn ++ " line " ++ (show line) ++ " column " ++ (show col)

printTokenType :: TokenType -> String
printTokenType s =
  case s of
    TokLit -> "Literal"
    TokId -> "Identifier"
    TokBinOp -> "Binary operator"
    TokLambda -> "Operator"
    TokDot -> "Operator"
    TokLet -> "Operator"
    TokDef -> "Operator"
    TokSemi -> "Semi-colon"
    TokParL -> "Left Parenthesis"
    TokParR -> "Right Parenthesis"

symNeedsSpace :: Token -> Token -> Bool
symNeedsSpace t1 t2 = case t1 of
  Token TokLet _ _ _ _ -> True
  Token TokDef _ _ _ _ -> True
  Token TokLambda _ _ _ _ -> False
  Token TokSemi _ _ _ _ -> False
  Token TokDot _ _ _ _ -> True
  Token TokBinOp _ _ _ _ -> True
  Token TokParL _ _ _ _ -> False
  _ -> case t2 of
    Token TokBinOp _ _ _ _ -> True
    Token TokDef _ _ _ _ -> True
    Token TokId _ _ _ _ -> True
    Token TokLit _ _ _ _ -> True
    _ -> False



symAddSpace :: Token -> Token
symAddSpace (Token a b sym c d) = (Token a b (sym ++ " ") c d)

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
  foldr (++) "" (addPadMap "\n    " (map printTokenInfo ts))

-- Prints a list of tokens as you would see them in source (String form)
printTokenListAsStmt :: [Token] -> String
printTokenListAsStmt ts =
  let
    tkn_sym = map getTokenSymbol $ spaceTknSym ts
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
  where l = (\(Token _ _ _ line _) -> line)

-- Tokenizes the source and prints the results
printTokenization :: String -> String -> String
printTokenization fn src =
  let
    tk_src = tokenizeSource fn src
  in fn ++ (foldr (++) "" $
    map (\t -> "\n  " ++ (printTokenListAsStmt t) ++ (printTokenList t)) tk_src)
