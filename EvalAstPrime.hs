module EvalAstPrime
(
   eval
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
vars (Ast.Delta _ lhs rhs) = (vars lhs ) ++ (vars rhs)
vars (Ast.Lambda (Ast.Var val) body) = (val:(vars body))
vars (Ast.Application lhs rhs) = (vars lhs) ++ (vars rhs)
vars (Ast.Exprs (h:tl)) = (vars h) ++ (vars (Ast.Exprs tl))
vars (Ast.Exprs []) = []
-- this will cover Unit and Nat
vars _ = []

-- generate a fresh variable
freshVar :: Ast.Expression -> Ast.Expression
freshVar expr =
   Ast.Var (minFreeString active)   
   where active = nub (vars expr)

-- return a list of free variables in a term
freeVars :: Ast.Expression -> [Ast.Expression]
freeVars (Ast.Var val) = [Ast.Var val]
freeVars (Ast.Nat _) = []
freeVars (Ast.Delta _ lhs rhs) = (freeVars lhs) ++ (freeVars rhs)
freeVars (Ast.Lambda param body) = filter (isEquivVar param) (freeVars body)
freeVars (Ast.Application lhs rhs) = (freeVars lhs) ++ (freeVars rhs)
freeVars (Ast.Exprs (h:tl)) = (freeVars h) ++ (freeVars (Ast.Exprs tl))
freeVars (Ast.Exprs []) = []
freeVars Ast.Unit = []

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
substitution n (Ast.Var x) (Ast.Var val) = 
   if x == val
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
substitution n x (Ast.Exprs (h:tl)) = 
   substitution n x (Ast.Exprs tl)
   where r = substitution n x h
-- No substitution for simple expressions
substitution _ _ (Ast.Exprs [] ) = Ast.Exprs []
substitution _ _ Ast.Unit = Ast.Unit

-- beginings of applications
apply :: Ast.Expression -> Ast.Expression -> Ast.Expression
apply ( Ast.Lambda param body ) rhs = 
   substitution rhs param body 
-- can't apply if left side is not a lambda
apply lhs rhs = Ast.Application lhs rhs   

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
evaluate ( Ast.Application lhs rhs ) = 
   apply lhs' rhs'
   where rhs' = evaluate rhs 
         lhs' = evaluate lhs  
evaluate ( Ast.Exprs (h:tl) ) =
   if length tl == 0
      then evaluate h
      else evaluate ( Ast.Exprs tl )
evaluate ( Ast.Exprs [] ) = Ast.Exprs []
-- simple expressions just require the identity function
evaluate x = x
