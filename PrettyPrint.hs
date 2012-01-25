module PrettyPrint
( printToken
, printTokenInfo
) where

import Tokenizer

printTokenInfo :: Token_info -> String
printTokenInfo (Token_info fn sym line col _) =
  "'" ++ sym ++ "'" ++ " in file " ++ fn ++ " line " ++ (show line) ++ " column " ++ (show col)



printToken :: Token -> String
printToken (Token_identifier info) =
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
  "Terminal " ++ (printTokenInfo info) ++ ".\n"



tokensToSource :: [Tokens] ->
tokensToSource ts = 
