module Evaluate( eval ) where

import Ast



eval :: Ast -> Ast

-- Evaluate arithmetic
eval (Application (Application (Constant (Primitive op val)) ast) ast') =
  case (eval_ast,eval_ast') of
    (Constant (IntCst a),Constant (IntCst b)) | elem op ["+","-","*","/"]
      -> deltaRule op a b
    (_,_) -> (Application (Application (Constant (Primitive op val)) eval_ast) eval_ast')
  where eval_ast = eval ast
        eval_ast' = eval ast'

eval (Application (Lambda sym term) ast)
  | isValue ast' = eval (betaReduction sym ast' term)
  | otherwise = error "Free variable found in [ast]"
  where ast' = eval ast

eval (Let (Unique _) _ ast) = eval ast

eval (Let (Identifier sym) term ast) =
  let value = eval term in
  (betaReduction sym value ast)

eval ast = ast

-- Beta reduction
-- Notes:
--  -When descending into a lambda in which the bound variable is that of the
--   variable being reduced, beta reduction terminates
--  -There are two triggers to a beta reduction, let and lambda (in eval)
--  -FIXME Redo this as a case expression
betaReduction :: String -> Ast -> Ast -> Ast
betaReduction sym expr (Let name t1 t2) =
  Let name (betaReduction sym expr t1) (betaReduction sym expr t2)
betaReduction sym expr (Application t1 t2) =
  Application (betaReduction sym expr t1) (betaReduction sym expr t2)
betaReduction sym expr e@(Lambda lamSym lamExpr)
  | lamSym == sym = e
  | otherwise = (Lambda lamSym (betaReduction sym expr lamExpr))
betaReduction sym expr e@(Variable (Identifier sym'))
  | sym == sym' = expr
  | otherwise = e
betaReduction sym expr other = other


-- delta rules for optimized arithmetic
deltaRule :: String -> Integer -> Integer -> Ast
deltaRule "+" a b = Constant (IntCst (a + b))
deltaRule "-" a b = Constant (IntCst (a + b))
deltaRule "*" a b = Constant (IntCst (a + b))
deltaRule "/" a b = Constant (IntCst (quot a b))
deltaRule op _ _ = error ("Passed unrecognized operation: " ++ op ++ "\n")

-- Predicate defining if a term is a variable
-- the meaning of variable here is that of lambda calculus and, therefore does
-- not refer to variable names
-- FIXME - applications can be variables
isValue :: Ast -> Bool
isValue (Constant _) = True
isValue (Lambda _ _) = True
isValue _ = False