module Ast
( Operator(..)
, Program(..)
, Expression(..)
, Type(..)
) where

import qualified ParseTree as PT

data Type = 
   Nat
   | Func Type Type
   deriving (Show,Eq)

data Operator = 
   Add 
   | Minus 
   | Mult 
   | Div 
   | App 
   deriving (Show,Eq)

data Program = 
   Prog [Expression]
   | ErrProg
   deriving (Show,Eq)

data Expression = 
   Id String
   | Num Int
   | Let Expression Expression
   | Lambda Expression Expression
   | Unary Operator Expression
   | Binary Operator Expression Expression
   | Compound [Expression]
   | ErrExpr
   deriving (Show,Eq) 

-- trnasform a prgram
transform :: PT.Program -> Program
transform ( PT.Prog (h:tl) ) = Prog transform (h:tl)
transform ( PT.Prog [] ) = Prog []
transform ( PT.ErrPrg _ ) = ErrProgram

-- transform a statement
transform ((PT.Stmt expr _ ):tl) = (transform expr : transform tl)
transform [] = []

transform ( PT.Id tok ) =
transform ( PT.Nat tok ) = 
transform ( PT.Let _ identifier _ src_expr ) = 
transform ( PT.Lambda _ id _ body ) =
transform ( PT.Binary operator lhs_expr rhs_expr ) =
transform ( PT.Application lhs_expr rhs_expr ) = Appl
transform ( PT.Compound _ (exprs) _ = Compound transform (fst unzip exprs)
