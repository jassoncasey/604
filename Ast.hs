module Ast
( Operator(..)
, Program(..)
, Expression(..)
, Type(..)
, transformProg
, transformStmts
, transformStmt
, transformExpr
, getStrProgram
, getStrSProgram
, getStrExpression
, getStrSExpression
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

-- child is application predicate
isApp :: Expression -> Bool
isApp ( Application _ _ ) = True
isApp _ = False

-- application node helper
lhsApp :: Expression -> Expression
lhsApp ( Application lhs _ ) = lhs
lhsApp _ = ErrExpr
rhsApp :: Expression -> Expression
rhsApp ( Application _ rhs ) = rhs
rhsApp _ = ErrExpr

-- simple transform that updates left associativity to application
leftAssociate :: Expression -> Expression
leftAssociate ( Let target source ) = 
   ( Let target ( leftAssociate source ) )
leftAssociate ( Lambda target source ) = 
   ( Lambda target ( leftAssociate source ) )
leftAssociate ( Binary op lhs rhs ) = 
   ( Binary op ( leftAssociate lhs ) ( leftAssociate rhs ) )
leftAssociate ( Application lhs rhs ) =
   if isApp rhs
      then leftAssociate ( Application ( Application ( lhs ) ( lhsApp rhs ) ) 
                                       ( rhsApp rhs ) ) 
      else ( Application (leftAssociate lhs) (leftAssociate rhs) ) 
leftAssociate ( Compound exprs ) = 
   ( Compound ( leftAssociateExprs exprs ) )
leftAssociate x = x

-- compound association helper
leftAssociateExprs :: [Expression] -> [Expression]
leftAssociateExprs (h:tl) = (leftAssociate h ):(leftAssociateExprs tl )
leftAssociateExprs [] = []

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

-- simple program printer
getStrSProgram :: Program -> String
getStrSProgram ( Prog exprs ) = 
   "(\n" ++ ( getStrSLines exprs ) ++ ")\n"
getStrSProgram _ = "Program errored ..."

-- program helper function to print lines
getStrSLines :: [Expression] -> String
getStrSLines (h:tl) =
   "\t" ++ ( getStrSExpression h ) ++ ";\n" ++ ( getStrSLines tl )
getStrSLines [] = ""

-- simple expression printer
getStrSExpression :: Expression -> String
getStrSExpression ( Id str ) = str
getStrSExpression ( Num val ) = show val
getStrSExpression ( Let target source ) = 
   "( let " ++ ( getStrSExpression target ) ++ " = " ++ 
      ( getStrSExpression source ) ++ " )"
getStrSExpression ( Lambda param body ) =
   "( \\" ++ ( getStrSExpression param ) ++ "." ++ 
      ( getStrSExpression body ) ++ " )"
getStrSExpression ( Binary op lhs rhs ) =
   "( " ++ ( getStrSExpression lhs ) ++ ( getStrOp op ) ++ 
      ( getStrSExpression rhs ) ++ " )"
getStrSExpression ( Application lhs rhs ) =
   "( " ++ ( getStrSExpression lhs ) ++ " " ++ ( getStrSExpression rhs ) ++ " )"
getStrSExpression ( Compound exprs ) =
   "( " ++ ( getStrSCompound exprs ) ++ " )"
getStrSExpression _ = "Unknown expression"

--getStrSProgram :: Program -> [String]
--getStrSProgram 

-- simple compound statement printer
getStrSCompound :: [Expression] -> String
getStrSCompound (h:tl) =
   if length tl == 0
      then getStrSExpression h
      else ( getStrSExpression h ) ++ ";" ++ ( getStrSCompound tl )
getStrSCompound [] = ""


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
transformStmt ( PT.Stmt expr _ ) = leftAssociate ( transformExpr expr )
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
