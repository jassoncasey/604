module Evaluate( evaluate, evalStep, isValue, applyN ) where

import Ast

-- This version is flawed. It only works for simple lambdas. What we need to do
-- is bring back contexts

--type ValueContext = [(String,Ast)]

applyN :: (a -> a) -> a -> Int -> a
applyN _ a 0 = a
applyN f a n = applyN f (f a) (n-1)

-- Really crappy way to do this
evaluate :: Ast -> Ast
evaluate t
  | t==t' = t
  | otherwise = evaluate t'
  where t' = evalStep t


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
  where value = evaluate ast

evalStep app@(Application t1 t2)
  | isValue t1 && isValue t2 = app
  | otherwise = evalStep (Application (evaluate t1) (evaluate t2))

evalStep (Let (Unique _) _ ast) = evalStep ast

evalStep (Let (Identifier sym) term ast) =
  let value = evalStep term in
  betaReduction sym value ast

-- Once outer is in normal form, descend
evalStep (Lambda sym t) = Lambda sym (evalStep t)

evalStep t = t

-- Put all rhs of applications to normal form
{-evalAppArgs :: Ast -> Maybe Ast
evalAppArgs (Application t1 t2)
  | isValue v = Maybe.Just (Application (evalAppArgs t1) v)
  | otherwise = Maybe.Nothing -- Error! Means we couldn't get this in nf
  where v = evaluate t2
-- If the next term isn't an application, don't do anything
evalAppArgs t = Maybe.Just t-}

-- special function to see if a term can be applied to another
{-canApply :: Ast -> Bool
canApply (Application t1 t2) = canApply t1
canApply (Lambda _ _) = True
canApply _ = False

apply :: Ast -> Ast -> Ast
apply (Application t1 t2) v = Application (apply t1 v) t2
apply (Lambda sym t) v = betaReduction sym v t
apply t1 v = Application t1 v-}

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
betaReduction _ _ anything_else = anything_else


-- delta rules for optimized arithmetic
deltaRule :: String -> Integer -> Integer -> Ast
deltaRule "+" a b = Constant (IntCst (a + b))
deltaRule "-" a b = Constant (IntCst (a - b))
deltaRule "*" a b = Constant (IntCst (a * b))
deltaRule "/" a b = Constant (IntCst (quot a b))
deltaRule op _ _ = error ("Passed unrecognized operation: " ++ op ++ "\n")


-- Predicate defining if a term is a value
-- rename to is term
-- the meaning of variable here is that of lambda calculus and, therefore does
-- not refer to variable names
-- FIXME - applications can be values when applied to 
-- FIXME - rename to is variable
isValue :: Ast -> Bool
isValue (Constant (IntCst _)) = True
--Are there lambdas that aren't values?
isValue l@(Lambda _ _) = isClosedTerm [] l
isValue (Application (Lambda _ _) _) = False
isValue (Application t1 t2) = isValue t1 && isValue t2
isValue (Variable _) = True
isValue _ = False

-- predicate to determine if an AST represents a constructor
{-isConstructor :: Ast -> Int -> Bool
isConstructor (Application t1 _) n = isValue t2 && isConstructor t1 (n + 1)
isConstructor (Constant (Constructor _ m)) n = n <= m
isConstructor _ _ = False-}

isClosedTerm :: [String] -> Ast -> Bool
isClosedTerm vars (Lambda sym t) = isClosedTerm (sym:vars) t
isClosedTerm vars (Application t1 _) =
  isClosedTerm vars t1 && isClosedTerm vars t1
isClosedTerm vars (Variable (Identifier sym)) = elem sym vars
isClosedTerm _ _ = True