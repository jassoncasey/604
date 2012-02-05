module AstPrime
(
   Operator(..),
   Expression(..),
   transformAst,
   getStrExpression,
   getStrAstPrime
) where

import qualified Ast as Ast

data Operator =
   Add
   | Minus
   | Mult
   | Div
   | NoOp
   | Exception
   deriving (Show,Eq)

data Expression =
   Var String
   | Nat Int
   | Delta Operator Expression Expression
   | Lambda Expression Expression
   | Application Expression Expression
   | Exprs [Expression]
   | Unit
   deriving (Show,Eq)

-- simple operator string formatter
getStrOp :: Operator -> String
getStrOp Add   = "+"
getStrOp Minus = "-"
getStrOp Mult  = "*"
getStrOp Div   = "/"
getStrOp NoOp  = " noop "
getStrOp Exception = " excpetion "

-- simple Ast operator transform
getOp :: Ast.Operator -> Operator
getOp Ast.Add     = Add
getOp Ast.Minus   = Minus
getOp Ast.Mult    = Mult
getOp Ast.Div     = Div
getOp _           = NoOp    -- this should never happen

-- simple expression string formatter
getStrExpression :: Expression -> String
getStrExpression (Var val) = val
getStrExpression (Nat val) = show val
getStrExpression (Delta op lhs rhs) =
   "(" ++  (getStrOp op ) ++ " " ++ 
            (getStrExpression lhs) ++ " " ++ 
            (getStrExpression rhs) ++")"
getStrExpression (Lambda param body) =
   "(" ++ "\\" ++   (getStrExpression param) ++ "." ++
                     (getStrExpression body) ++ ")"
getStrExpression (Application lhs rhs) =
   "(" ++  (getStrExpression lhs) ++
            (getStrExpression rhs) ++ ")"
getStrExpression (Exprs exprs) =
   "(" ++ (getStrExprs exprs) ++ ")"
getStrExpression Unit = "unit"

getStrAstPrime :: Expression -> String
getStrAstPrime expr = (getStrExpression expr) ++ "\n"

-- simple expression list string formatter
getStrExprs :: [Expression] -> String
getStrExprs (h:tl) = 
   if length tl == 0
      then (getStrExpression h)
      else (getStrExpression h) ++ "; " ++
            (getStrExprs tl)
getStrExprs [] = ""

-- Entry point for transform
transformAst :: Ast.Program -> Expression
transformAst (Ast.Prog exprs) = Exprs (transformAstExprs exprs)
transformAst _ = Unit

-- Roll through the program and transform all lets to lambda
transformAstExprs :: [Ast.Expression] -> [Expression]
transformAstExprs ((Ast.Let target source):tl) =
   [Application (Lambda param body) applied]
   where body = Exprs (transformAstExprs tl)
         param = transformAstExpr target
         applied = (transformAstExpr source)
transformAstExprs (h:tl) = 
   ((transformAstExpr h):(transformAstExprs tl))
transformAstExprs [] = []

-- simple single expression transform
transformAstExpr :: Ast.Expression -> Expression
transformAstExpr (Ast.Id val) = Var val
transformAstExpr (Ast.Num val) = Nat val
-- note the absence of let
transformAstExpr (Ast.Lambda param body ) =
   Lambda (transformAstExpr param) (transformAstExpr body )
transformAstExpr (Ast.Binary op lhs rhs ) =
   Delta (getOp op) (transformAstExpr lhs) (transformAstExpr rhs)
transformAstExpr (Ast.Application lhs rhs ) =
   Application (transformAstExpr lhs) (transformAstExpr rhs)
-- simplify compounds to a list of expressions
transformAstExpr (Ast.Compound exprs) = Exprs (transformAstExprs exprs)
transformAstExpr _ = Unit

