module Ast
( Operator(..)
, Program(..)
, Expression(..)
, Type(..)
) where

data Type = 
   Nat
   | Func Type Type
   deriving (Show,Eq)

data Operator = Add | Minus | Mult | Div | App deriving(Show,Eq)

data Program = Prog Type [Expression] Int

data Expression = 
   Id Type String
   | Num Type Int
   | Let Type Expression Expression
   | Lambda Type Expression Expression
   | Unary Type Operator Expression
   | Binary Type Operator Expression Expression
   | Compound Type [Expression]
   deriving (Show,Eq) 
