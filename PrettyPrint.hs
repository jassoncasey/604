module PrettyPrint
( printToken
, printTokenInfo
, printParse
, sourceToStatements
) where

import Tokenizer
import ListAux

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



sourceToStatements :: String -> [String]
sourceToStatements source = splitAfter (==';') source

-- Prints all of the tokens in a token list
printTokensVerbose :: [Token] -> String
printTokensVerbose ts = map (\x -> "\n    " ++ x) (map printToken ts)

k ::

printParse :: String -> String -> String
printParse fn source = "Parsing " ++ fn ++ (foldr (++) "" (map (\x -> ('\n':' ':' ':x)) $ sourceToStatements source))
