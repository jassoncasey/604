-- Copyright (C) 2012, Texas A&M University.  All rights reserved.
-- Written by Gabriel Dos Reis.

module Ast where
import qualified Parsing
import qualified Lexing

data Ast = 
    Constant CstData
  | Variable Name
  | Lambda String Ast
  | Application Ast Ast
  | Let Name Ast Ast
  deriving Show

data CstData =
    IntCst Integer
  | Constructor String Int
  | Primitive String Int
  deriving Show

data Name = 
    Identifier String -- source level identifier
  | Unique Int        -- unique internal identifier
  deriving (Eq, Show)

data TransformerState = 
  TransformerState {
     bound_ids :: [String],
     next_unique :: Int
  }

program :: Parsing.Program -> Maybe Ast
program (Parsing.Program xs) = 
  if null xs then Nothing
  else Just (fst $ from_parse_trees (mkTransformerState [] 0) trees)
    where trees = map (\x -> case x of Parsing.Statement y -> y) xs


from_parse_trees :: TransformerState -> [Parsing.ParseTree] -> 
                        (Ast,TransformerState)
from_parse_trees st [x] = 
  from_parse_tree st x

from_parse_trees st (x:xs) = 
  let (y, st') = from_parse_tree st x
      (ys', _) = from_parse_trees st' xs
  in case y of 
     Let var initial (Variable var') | var == var' -> (Let var initial ys',st')
     _ -> let (var,st'') = gensym st'
          in (Let var y ys',st'')
from_parse_trees _ [] = error "There shouldn't be an error here..."

from_parse_tree :: TransformerState -> Parsing.ParseTree
  -> (Ast, TransformerState)
from_parse_tree st x =
  case x of 
    Parsing.Atomic t -> (from_atomic st t, st)
    Parsing.CompoundExpression _ x' x'' -> from_parse_trees st [x',x'']
    Parsing.Application f a -> from_application st f a
    Parsing.Factor t x' x'' -> from_builtin st t x' x''
    Parsing.Term t x' x'' -> from_builtin st t x' x''
    Parsing.Lambda t x' ->  from_lambda st t x'
    Parsing.Let t x' -> from_let st t x'

from_application :: TransformerState -> Parsing.ParseTree -> Parsing.ParseTree
  -> (Ast, TransformerState)
from_application st f a = 
  let (x,st') = from_parse_tree st f
      (y,st'') = from_parse_tree st' a
  in (Application x y, st'')

from_builtin :: TransformerState -> Lexing.Token -> Parsing.ParseTree
  -> Parsing.ParseTree -> (Ast, TransformerState)
from_builtin st t x y = 
  let (x', _{-st'-}) = from_parse_tree st x
      (y', st'') = from_parse_tree st y
      op = from_atomic st t 
  in (Application (Application op x') y', st'')

from_lambda :: TransformerState -> Lexing.Token -> Parsing.ParseTree
  -> (Ast, TransformerState)
from_lambda st (Lexing.Token Lexing.Identifier var _) x = 
  let (y, st') = from_parse_tree (bind_variable st var) x
  in (Lambda var y, st')
--from_lambda _ (_ Lexing.Junk _ _) _ = error "Ast error: got junk???"
from_lambda _ _ _ = error "Ast error: got unexpected result from lambda expr."

from_let :: TransformerState -> Lexing.Token -> Parsing.ParseTree
  -> (Ast, TransformerState)
from_let st (Lexing.Token Lexing.Identifier var _) x = 
  let (y, st') = from_parse_tree st x
      z = Identifier var
  in (Let z y (Variable z), bind_variable st' var)
--from_let _ (_ Lexing.Junk _ _) _ = error "Ast error: got junk???"
from_let _ _ _ = error "Ast error: got unexpected result from lambda expr."

from_atomic :: TransformerState -> Lexing.Token -> Ast
from_atomic st (Lexing.Token tt s _) = 
  case tt of
    Lexing.Identifier -> handle_identifier st s
    Lexing.Operator o -> Constant (Primitive o 2)
    Lexing.Natural n -> Constant (IntCst n)
    _ -> error ("spl internal error: unexpected " ++ show tt)

handle_identifier :: TransformerState -> String -> Ast
handle_identifier st s =
  if s `elem` (bound_ids st) then Variable (Identifier s)
  else if s == "cons" then Constant (Constructor s 2)
  else if s == "nil" then Constant (Constructor s 0)
  else Variable (Identifier s)

bind_variable :: TransformerState -> String -> TransformerState
bind_variable st v = 
  mkTransformerState (v : bound_ids st) (next_unique st)

mkTransformerState :: [String] -> Int -> TransformerState
mkTransformerState ids n = 
  TransformerState { bound_ids = ids, next_unique = n }

gensym :: TransformerState -> (Name, TransformerState)
gensym st = 
  (Unique $ next_unique st, 
    mkTransformerState (bound_ids st) (1 + (next_unique st)))
