module PrettyPrint
( printTokenization
) where

import Tokenizer
import ListAux
--import Parse -- This needs to merge into this file

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


{-printToken :: Token -> String
printToken (Token info) =
  "Identifier " ++ (printTokenInfo info) ++ ".\n"
printToken (Token_natural _ info) = 
  "Literal " ++ (printTokenInfo info) ++ ".\n"
printToken (Token_operator info) = 
  "Binary operator " ++ (printTokenInfo info) ++ ".\n"
printToken (Token_left_parenth info) = 
  "Left parenthesis " ++ (printTokenInfo info) ++ ".\n"
printToken (Token_right_parenth info) = 
  "Right parenthesis " ++ (printTokenInfo info) ++ ".\n"
printToken (Token_lambda info) = 
  "Operator " ++ (printTokenInfo info) ++ ".\n"
printToken (Token_let info) = 
  "Keyword " ++ (printTokenInfo info) ++ ".\n"
printToken (Token_dot info) = 
  "Operator " ++ (printTokenInfo info) ++ ".\n"
printToken (Token_define info) = 
  "Binary operator " ++ (printTokenInfo info) ++ ".\n"
printToken (Token_terminal info) = 
  "Terminal " ++ (printTokenInfo info) ++ ".\n"-}


addPadMap :: String -> [String] -> [String]
addPadMap padding ts = map (\x -> padding ++ x) ts

-- Prints all of the tokens in a token list
printTokenList :: [Token] -> String
printTokenList ts =
  foldr (++) "" (addPadMap "\n    " (map printTokenInfo ts))

printTokenListAsStmt :: [Token] -> String
printTokenListAsStmt ts =
  let
    tkn_sym = map getTokenSymbol ts
  in foldr (++) "" tkn_sym

printTokenization :: String -> String -> String
printTokenization fn src =
  let
    tk_src = tokenizeSource fn src -- type = [[Token]]
  in fn ++ (foldr (++) "" $
    addPadMap "\n" (map show tk_src))
    --map printTokenList tk_src)
    --map printTokenListAsStmt tk_src)


{-

statementsAndTokens :: String -> String -> ([String], [[Token]])
statementsAndTokens fn source =
  (sourceToStatements source, tokenStatements fn source)

printTokenizedFile :: String -> String -> String
printTokenizedFile fn source =
  let
    (st, tkn) = statementsAndTokens fn source
    tkn_str = map printTokenList tkn
    st' = addPadMap "\n  " st
  in
  "Parsing " ++ fn ++ (foldr (++) "" $ map (\(x,y) -> x ++ y) (zip tkn_str st'))
-}



--(foldr (++) "" (map (\x -> ('\n':' ':' ':x)) $ sourceToStatements source))

