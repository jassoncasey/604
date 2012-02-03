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

-- simple operator to string helper
getStrOp :: Operator -> String
getStrOp op =
   case op of
      Add -> "+"
      Minus -> "-"
      Mult -> "*"
      Div -> "/"
      _ -> "?"

-- simple program printer
getStrProgram :: Program -> String
getStrProgram ( Prog exprs ) = 
   getStrLines exprs
getStrProgram _ = "Program errored ..."

-- program helper function to print lines
getStrLines :: [Expression] -> String
getStrLines (h:tl) =
   ( getStrExpression h ) ++ ";\n" ++ ( getStrLines tl )
getStrLines [] = ""

-- simple expression printer
getStrExpression :: Expression -> String
getStrExpression ( Id str ) = str
getStrExpression ( Num val ) = show val
getStrExpression ( Let target source ) = 
   "let " ++ ( getStrExpression target ) ++ " = " ++ ( getStrExpression source )
getStrExpression ( Lambda param body ) =
   "\\" ++ ( getStrExpression param ) ++ "." ++ ( getStrExpression body )
getStrExpression ( Binary op lhs rhs ) =
   ( getStrExpression lhs ) ++ ( getStrOp op ) ++ ( getStrExpression rhs ) 
getStrExpression ( Application lhs rhs ) =
   ( getStrExpression lhs ) ++ " " ++ ( getStrExpression rhs ) 
getStrExpression ( Compound exprs ) =
   "( " ++ ( getStrCompound exprs ) ++ " )"
getStrExpression _ = "Unknown expression"

-- simple compound statement printer
getStrCompound :: [Expression] -> String
getStrCompound (h:tl) =
   if length tl == 0
      then getStrExpression h
      else ( getStrExpression h ) ++ ";" ++ ( getStrCompound tl )
getStrCompound [] = ""

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
