module Evaluate( evaluate, evalStep, ValueContext(..) ) where

import Ast

-- This version is flawed. It only works for simple lambdas. What we need to do
-- is bring back contexts

type ValueContext = [(String,Ast)]

-- Really crappy way to do this
evaluate :: Ast -> Ast
evaluate t
  | isValue t = t
  | otherwise = evaluate $ evalStep t


evalStep :: Ast -> Ast

-- Evaluate arithmetic
evalStep (Application (Application (Constant (Primitive op val)) ast) ast') =
  case (eval_ast,eval_ast') of
    (Constant (IntCst a),Constant (IntCst b)) | elem op ["+","-","*","/"]
      -> deltaRule op a b
    (_,_) -> (Application (Application (Constant (Primitive op val)) eval_ast) eval_ast')
  where eval_ast = evalStep ast
        eval_ast' = evalStep ast'

evalStep (Application (Lambda sym term) ast)
  | isValue value = evalStep (betaReduction sym value term)
  | otherwise = error "Free variable found in [ast]"
  where value = evalStep ast

-- should check for a value
evalStep (Application t1 t2)
  | canApply t1 && isValue value = evalStep $ apply t1 value
  | otherwise = Application t1 value
  where value = evalStep t2
        

evalStep (Let (Unique _) _ ast) = evalStep ast

evalStep (Let (Identifier sym) term ast) =
  let value = evalStep term in
  betaReduction sym value ast

evalStep t = t

-- special function to see if a term can be applied to another
canApply :: Ast -> Bool
canApply (Application t1 t2) = canApply t1
canApply (Lambda _ _) = True
canApply _ = False

apply :: Ast -> Ast -> Ast
apply (Application t1 t2) v = Application (apply t1 v) t2
apply (Lambda sym t) v = betaReduction sym v t
apply t1 v = Application t1 v

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
deltaRule "-" a b = Constant (IntCst (a - b))
deltaRule "*" a b = Constant (IntCst (a * b))
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