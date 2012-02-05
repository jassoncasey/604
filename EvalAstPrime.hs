module EvalAstPrime
(
   eval
   , freeVars
) where

import qualified AstPrime as Ast
import CompilerUtils
import Data.List

-- simple delta computation unit
deltaCompute :: Ast.Operator -> Ast.Expression -> Ast.Expression -> Ast.Expression
deltaCompute op (Ast.Nat lhs ) (Ast.Nat rhs) =
   case op of 
      Ast.Add     -> Ast.Nat ( lhs + rhs )
      Ast.Minus   -> Ast.Nat ( lhs - rhs )
      Ast.Mult    -> Ast.Nat ( lhs * rhs )
      Ast.Div     -> Ast.Nat ( quot lhs rhs ) 
      _           -> Ast.Unit
deltaCompute _ _ _ = Ast.Unit

-- check for variable equivilance 
isEquivVar :: Ast.Expression -> Ast.Expression -> Bool
isEquivVar (Ast.Var l) (Ast.Var r) = l == r
isEquivVar _ _= False

-- generate a list of variables in use
vars :: Ast.Expression -> [String]
vars (Ast.Var val) = [val]
vars (Ast.Delta _ lhs rhs) = nub ((vars lhs) ++ (vars rhs))
vars (Ast.Lambda (Ast.Var val) body) = nub (val:(vars body))
vars (Ast.Application lhs rhs) = nub((vars lhs) ++ (vars rhs))
vars (Ast.Exprs (h:tl)) = nub ((vars h) ++ (vars (Ast.Exprs tl)))
vars (Ast.Exprs []) = []
-- this will cover Unit and Nat
vars _ = []

-- generate a fresh variable
freshVar :: Ast.Expression -> Ast.Expression
freshVar expr =
   Ast.Var (minFreeString (vars expr))   

-- return a list of free variables in a term
freeVars :: Ast.Expression -> [Ast.Expression]
freeVars (Ast.Var val) = [Ast.Var val]
freeVars (Ast.Delta _ lhs rhs) = nub ((freeVars lhs) ++ (freeVars rhs))
freeVars (Ast.Lambda param body) = filter (/= param) (freeVars body)
freeVars (Ast.Application lhs rhs) = nub ((freeVars lhs) ++ (freeVars rhs))
freeVars (Ast.Exprs (h:tl)) = nub ((freeVars h) ++ (freeVars (Ast.Exprs tl)))
freeVars (Ast.Exprs []) = []
-- this will cover Unit and Nat
freeVars _ = []

-- implement a lambda substitution where a rewrite is necessary
lambdaSubFresh :: Ast.Expression -> Ast.Expression -> Ast.Expression -> 
                     Ast.Expression -> Ast.Expression
lambdaSubFresh n x param body =
   Ast.Lambda z body''
   where z = freshVar (Ast.Exprs [n, body])
         body' = substitution z param body
         body'' = substitution n x body'

-- implement lambda substitution rules
lambdaSub :: Ast.Expression -> Ast.Expression -> Ast.Expression -> 
                     Ast.Expression -> Ast.Expression
lambdaSub n x param body =
   if not (elem x fv_body)
      then Ast.Lambda param body
      else if (elem param fv_n)
         then lambdaSubFresh n x param body
         else Ast.Lambda param (substitution n x body)
   where fv_body = freeVars body
         fv_n    = freeVars n

-- core of lambda calculus substitution
substitution :: Ast.Expression -> Ast.Expression -> Ast.Expression -> 
                     Ast.Expression
-- [N/x]x=N, [N/x]a=a
substitution n x (Ast.Var val) = 
   if x == (Ast.Var val)
      then n
      else Ast.Var val
-- [N/x]a=a
substitution _ _ (Ast.Nat val) = Ast.Nat val
-- [N/x](P Q)=([N/x]P [N/x]Q)
substitution n x (Ast.Application lhs rhs) = 
   Ast.Application (substitution n x lhs) (substitution n x rhs)
-- [N/x](delta op P Q)=(delta op [N/x]P [N/x]Q)
substitution n x (Ast.Delta op lhs rhs) =
   Ast.Delta op (substitution n x lhs) (substitution n x rhs)
-- [N/x]\x.P ....
substitution n x (Ast.Lambda param body) =
   if isEquivVar x param
      -- =\x.P
      then (Ast.Lambda param body)
      -- more complicated
      else lambdaSub n x param body
-- [N/x] t:tl = [N/x]t):[N/x]tl
substitution n x (Ast.Exprs exprs) = Ast.Exprs (substitutionList n x exprs)
-- No substitution for simple expressions
substitution _ _ Ast.Unit = Ast.Unit

-- substitution list helper
substitutionList :: Ast.Expression -> Ast.Expression -> [Ast.Expression] -> 
                     [Ast.Expression]
substitutionList n x (h:tl) = ((substitution n x h):(substitutionList n x tl))
substitutionList _ _ [] = []

-- beginings of applications
apply :: Ast.Expression -> Ast.Expression -> Ast.Expression
apply ( Ast.Lambda param body ) rhs = 
   substitution rhs param body
apply ( Ast.Exprs exprs ) rhs = Ast.Exprs (applyList exprs rhs)
-- can't apply if left side is not a lambda
apply lhs rhs = Ast.Application lhs rhs   

-- apply an application across a list
applyList :: [Ast.Expression] -> Ast.Expression -> [Ast.Expression]
applyList (h:tl) rhs = ((apply h rhs):(applyList tl) rhs)
applyList [] _ = []

eval :: Ast.Expression -> Ast.Expression
eval term =
   if result == result'
      then result
      else evaluate result'
   where result = evaluate term
         result' = evaluate result

-- spl evaluator
evaluate :: Ast.Expression -> Ast.Expression
evaluate ( Ast.Delta op lhs rhs ) = 
   deltaCompute op lhs' rhs'
   where lhs' = evaluate lhs
         rhs' = evaluate rhs
evaluate ( Ast.Lambda param body ) =
   Ast.Lambda param (evaluate body)
evaluate ( Ast.Application lhs rhs ) = 
   apply lhs' rhs'
   where rhs' = evaluate rhs 
         lhs' = evaluate lhs  
-- this is broken and should be fixed
evaluate ( Ast.Exprs exprs ) = Ast.Exprs (evalList exprs)
-- simple expressions just require the identity function
evaluate x = x

evalList :: [Ast.Expression] -> [Ast.Expression]
evalList (h:tl) = (evaluate h):(evalList tl)
evalList [] = []
