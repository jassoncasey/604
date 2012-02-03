module Ast
( Operator(..)
, Program(..)
, Expression(..)
, Type(..)
, transformProg
, transformStmts
, transformStmt
, transformExpr
) where

import qualified ParseTree as PT
import qualified Lexer as Lx

data Type = 
   Nat
   | Func Type Type
   deriving (Show,Eq)

data Operator = 
   Add 
   | Minus 
   | Mult 
   | Div 
   | Error
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
   | Binary Operator Expression Expression
   | Application Expression Expression
   | Compound [Expression]
   | ErrExpr
   deriving (Show,Eq) 

-- simple operator helper function
getOp :: String -> Operator
getOp operator =
   case operator of
      "+" -> Add
      "-" -> Minus
      "*" -> Mult
      "/" -> Div
      _ -> Error

-- trnasform a prgram
transformProg :: PT.Program -> Program
transformProg ( PT.Prog stmts ) = Prog ( transformStmts stmts )
transformProg ( PT.ErrPrg _ ) = ErrProg

-- statements to statement helper function
transformStmts :: [PT.Statement] -> [Expression]
transformStmts (h:tl) = ( ( transformStmt h ):( transformStmts tl ) )
transformStmts [] = []

-- transform a statement
transformStmt :: PT.Statement -> Expression
transformStmt ( PT.Stmt expr _ ) = transformExpr expr
transformStmt _ = ErrExpr

-- transform an expression
transformExpr :: PT.Expression -> Expression
transformExpr ( PT.Id ( Lx.Tok _ ( Lx.Lex str _ _ _) ) ) = Id str
transformExpr ( PT.Nat ( Lx.Tok _ ( Lx.Lex str _ _ _) ) ) = Num (read str)
transformExpr ( PT.Let _ target _ source ) = 
   Let ( transformExpr target ) ( transformExpr source )
transformExpr ( PT.Lambda _ target _ body ) =
   Lambda ( transformExpr target ) ( transformExpr body )
transformExpr ( PT.Binary ( Lx.Tok _ ( Lx.Lex operator _ _ _ ) ) lhs rhs ) =
   Binary ( getOp operator ) ( transformExpr lhs ) ( transformExpr rhs )
transformExpr ( PT.Application lhs rhs ) = 
   Application ( transformExpr lhs ) ( transformExpr rhs )
transformExpr ( PT.Compound _ exprs _ ) = 
   Compound ( transformCompound exprs )
transformExpr _ = ErrExpr

-- compound transform helper
transformCompound :: [ ( PT.Expression, Lx.Token ) ] -> [Expression]
transformCompound ( ( expr, _ ):tl ) = 
   ( ( transformExpr expr ):( transformCompound tl ) )
transformCompound [] = []
