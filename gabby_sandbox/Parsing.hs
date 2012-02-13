-- Copyright (C) 2012, Texas A&M University.  All rights reserved.
-- Written by Gabriel Dos Reis.

module Parsing (Program(..), Statement(..), ParseTree (..), Diagnostic(..),
                parse_program) where
import Lexing

newtype Program = Program [Statement]

newtype Statement = Statement ParseTree
  deriving Show

data ParseTree = 
    Atomic Token
  | CompoundExpression Token ParseTree ParseTree
  | Application ParseTree ParseTree
  | Factor Token ParseTree ParseTree
  | Term Token ParseTree ParseTree  
  | Lambda Token ParseTree
  | Let Token ParseTree

data Outcome a = 
    EndOfInput
  | Valid a [Token]
  | Error Diagnostic
  | Nada [Token]

data Diagnostic = 
    Missing TokenType ParseTree
  | Expected String [Token]
  | Unspecified [Token]
  deriving Show

parse_program :: [Token] -> (Program, [Diagnostic])
parse_program ts =
  go ts [] [] where
    go ts'' xs ds =
      case parse_statement ts'' of
        EndOfInput -> (Program $ reverse xs, reverse ds)
        Valid x ts' -> go ts' (x : xs) ds
        Error d -> (Program $ reverse xs, reverse (d : ds))
        Nada ts' -> (Program $ reverse xs, reverse ((Unspecified ts') : ds))

parse_statement :: [Token] -> Outcome Statement
parse_statement ts = 
  case parse_expression ts of
    EndOfInput -> EndOfInput
    Valid x ts' -> require (Separator ';') ts' x Statement
    Error d -> Error d
    Nada ts' -> Nada ts'

parse_expression :: [Token] -> Outcome ParseTree
parse_expression ts =
  case ts of 
    (Token (Keyword LambdaKey) _ _):ts' -> parse_lambda_tail ts'
    (Token (Keyword LetKey) _ _):ts' -> parse_let_tail ts'
    _ -> parse_term ts

parse_term :: [Token] -> Outcome ParseTree
parse_term ts = 
  parse_left_associative
    parse_factor "factor" ts [Operator "+", Operator "-"] Term

parse_factor :: [Token] -> Outcome ParseTree
parse_factor ts =
  parse_left_associative
    parse_application "application" ts [Operator "*", Operator "/"] Factor

parse_application :: [Token] -> Outcome ParseTree
parse_application ts = 
  case parse_primary ts of 
     Valid x ts' -> parse_application_tail ts' x
     y -> y

parse_primary :: [Token] -> Outcome ParseTree
parse_primary ts = 
  case ts of 
    [] -> EndOfInput
    (t@(Token (Natural _) _ _):ts') -> Valid (Atomic t) ts'
    (t@(Token Identifier _ _):ts') -> Valid (Atomic t) ts'
    (Token (Separator '(') _ _): ts' -> parse_compound_expression_tail ts'
    _ -> Nada ts

parse_compound_expression_tail :: [Token] -> Outcome ParseTree
parse_compound_expression_tail ts =
  case must parse_compound_expression ts "compound expression" of
    Valid x ts' -> require (Separator ')') ts' x id
    y -> y

parse_compound_expression :: [Token] -> Outcome ParseTree
parse_compound_expression ts = 
  parse_left_associative 
     parse_expression "expression" ts [Separator ';'] CompoundExpression


-- Apply `parser' to the token stream `ts' with the assumption
-- that parse trees associate to the left under operations in `ops',
-- yielding a parse tree contructed with `ctor'.
type BinTreeCtor = Token -> ParseTree -> ParseTree -> ParseTree

type MiniParser = [Token] -> Outcome ParseTree

parse_left_associative :: 
  MiniParser -> String -> [Token] -> [TokenType] -> 
     BinTreeCtor -> Outcome ParseTree

parse_left_associative parser msg ts ops ctor =
  case parser ts of
    Valid x ts' -> left_associate parser msg ts' ops ctor x
    z -> z

-- Main loop of parse_left_associative. All parameters have the
-- same meaning as there.  Additionally `x' is the parse tree
-- constructed so far.
left_associate :: ([Token] -> Outcome ParseTree) -> String -> [Token]
  -> [TokenType] -> (Token -> ParseTree -> ParseTree -> ParseTree) -> ParseTree
  -> Outcome ParseTree

left_associate parser msg ts ops ctor x =
  if null ts then Valid x ts
  else if token_type (head ts) `elem` ops then
     case must parser (tail ts) msg of
        Valid y ts' -> 
          left_associate parser msg ts' ops ctor (ctor (head ts) x y)
        z -> z
  else Valid x ts

parse_application_tail :: [Token] -> ParseTree -> Outcome ParseTree
parse_application_tail ts x = 
  case parse_primary ts of
     Valid y ts' -> parse_application_tail ts' (Application x y)
     Nada ts' -> Valid x ts'
     EndOfInput -> Valid x []
     y -> y

next_token_has_type :: [Token] -> TokenType -> Bool
next_token_has_type ts tt =
  not (null ts) && token_type (head ts) == tt

parse_binding :: [Token] -> TokenType ->
  (Token -> ParseTree -> ParseTree) -> Outcome ParseTree
parse_binding ts tt ctor =
  if next_token_has_type ts Identifier then
    let ts' = tail ts in
      if next_token_has_type ts' tt then
        case must parse_expression (tail ts') "expression" of
          Valid x ts'' -> Valid (ctor (head ts) x) ts''
          y -> y
      else Error $ Expected (show tt) ts'
  else Error $ Expected "identifier" ts
      
parse_lambda_tail :: [Token] -> Outcome ParseTree
parse_lambda_tail ts =
  parse_binding ts (Keyword PeriodKey) Lambda

parse_let_tail :: [Token] -> Outcome ParseTree
parse_let_tail ts =
  parse_binding ts (Keyword EqKey) Let

require :: TokenType -> [Token] -> ParseTree -> (ParseTree -> a) -> Outcome a
require tt ts x ctor =
  if next_token_has_type ts tt then Valid (ctor x) (tail ts)
  else Error (Missing tt x)

must :: ([Token] -> Outcome ParseTree) -> [Token] -> String -> Outcome ParseTree
must parser ts msg =
  case parser ts of
    x@(Valid _ _) -> x
    _ -> Error $ Expected msg ts

-----------------------------
-- showing off parse trees --
-----------------------------
instance Show ParseTree where
  show (Atomic t) = show t
  show (CompoundExpression _ x y) = 
    "(CompoundExpression " ++ show x ++ " " ++ show y ++ ")"
  show (Application t1 t2) =
    "(Application " ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Factor t _ y) = 
    "(" ++ show t ++ " " ++ show y ++ ")"
  show (Term t _ y) = 
    "(" ++ show t ++ " " ++ show y ++ ")"
  show (Lambda t x) =
    "(Lambda " ++ show t ++ " " ++ show x ++ ")"
  show (Let t x) =
    "(Let " ++ show t ++ " " ++ show x ++ ")"
