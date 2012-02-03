module JEval
(
   Scope,
   Environment
   emptyEnv,
   evaluateProg,
   evaluateExpr
) where

import qualified Ast as Ast

-- Simple local scopes and environment stack
type Scope = [ ( String,Ast.Expression ) ]
type Environment = [ Scope ]

-- Simple scope identifier lookup
scopeLookup :: String -> Scope -> ( Status, Ast.Expression )
scopeLookup identifier ( ( key, expr ):tl ) =
   if key == identifier
      then ( Success, expr )
      else scopeLookup identifier tl
scopeLookup identifier [] = ( Failure, Ast.ErrExpr )

-- need a lookup function for environment
environmentLookup :: String -> Environment -> Ast.Expression
environmentLookup identifier (h:tl) =
   case status of
      Success -> result
      Failure -> environmentLookup identifier tl
   where ( status, result ) = scopeLookup identifier h
environmentLookup identifier [] = Ast.ErrExpr

-- Environment for let bindings to pass around during evaluation
emptyEnv :: [ Scope ]
emptyEnv = [[]]

type Evaluation = ( Environment, Ast.Expression )

-- simple helper function for arithmetic delta computations
deltaCompute :: Ast.Operator Ast.Expression Ast.Expression
deltaCompute operator (Ast.Num lhs ) (Ast.Num rhs ) =
   case operator of
      Ast.Plus    -> Ast.Num ( lhs + rhs )
      Ast.Minus   -> Ast.Num ( lhs - rhs )
      Ast.Mult    -> Ast.Num ( lhs * rhs )
      Ast.Div     -> Ast.Num ( lhs / rhs )
      _           -> Ast.ErrExpr
deltaCompute operator lhs rhs = Ast.Binary operator lhs rhs

-- top-level evaluation routine
evaluateProg :: Ast.Program -> Ast.Expression
evaluateProg ( Ast.Prog exprs ) = evaluate ( emptyEnv, exprs )
evaluateProg _ = Ast.ErrExpr

-- helper function to evaluate a list of expressions
-- no new scope is created during expression list execution
evaluateExprs :: ( Environment, [Ast.Expression] ) -> 
                  ( Environment, Ast.Expression )
evaluateExprs ( env, ( h:tl ) ) =
   if length tl == 0
      then ( env', result )
      else evaluateExprs ( env', tl )
   where ( env', result ) = evaluateExpr env h
evaluateExprs ( env, [] ) = ( env, Ast.Empty )

-- evaluate a single expression
evaluateExpr :: ( Environment, Ast.Expression ) -> 
                  ( Environment, Ast.Expression )
-- lookup the identifier in the environment and return its expression
evaluateExpr ( env, Ast.Id identifier ) =
   ( env, environmentLookup identifier env )

-- evaluate the source and bind an identifier in the current scope
evaluateExpr ( env, Ast.Let target source ) =
   ( ( target, result ) : env, result )
   where ( env', result ) = evaluateExpr env source
   
evaluateExpr ( env, Ast.Binary operator lhs rhs ) =
   ( env'', deltaCompute operator lresult rresult )
   -- no more computations are possible once each
   -- term has been evaluated
   where ( env', lresult ) = evaluateExpr ( env, lhs )
   ( env'', rresult ) = evaluateExpr ( env', rhs )

evaluateExpr ( env, Ast.Application lhs rhs ) =
   -- this needs a little thought ... should evaluate in morning
   where ( env', result ) = evaluateExpr ( env, rhs )

-- simple list execution of compound expression
evaluateCompound ( env, ( h:tl ) ) =
   if length tl == 0
      then ( env, result )
      else evaluateCompound env' tl
   where ( env', result ) = evaluateExpr env h
evaluateCompound ( env, [] ) = ( env, Ast.ErrExpr )

-- push scope on enter, pop scope on leave
evaluateExpr ( env, Ast.Compound exprs ) =
   ( env, evaluateCompound ( []:env, exprs ) )

-- This satisfies all expressions with no evaluation rules
-- lambda and num
evaluateExpr x = x
