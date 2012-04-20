module Steve.Ast where

import Steve.Parser as P

-- Untyped AST
data Ast =
    Nil
  | Iden String
  | Lit Constant
  | Abs String Ast
  | App Ast Ast
  | If Ast Ast Ast
  | Let String Ast Ast deriving (Eq,Show)

data TAst =
    TIden Type String
  | TLit Type Constant
  | TAbs Type String TAst
  | TApp Type TAst TAst
  | TLet String TAst TAst deriving (Eq,Show)

data Type = TNat | TBool | TChar deriving (Eq,Show)

-- Creates a let node for each top level function
toAst :: [P.TopLevelFunc] -> Ast
toAst [] = Nil
toAst [func] = toAstFromTop func
toAst (f@(name,_,_,_):funcs) = Let name t $ toAst funcs
  where t = toAstFromTop f

-- Converts a top-level function into an Ast
toAstFromTop :: P.TopLevelFunc -> Ast
toAstFromTop (_, _, args, t) = argsToAbs args t

-- Converts a function body into a lambda calulus AST
argsToAbs :: [String] -> P.PTree -> Ast
argsToAbs [] t = fromPTreeToAst t
argsToAbs [x] t = Abs x $ fromPTreeToAst t
argsToAbs (x:xs) t = Abs x $ argsToAbs xs t

-- Convert a Parse tree to an AST
fromPTreeToAst :: P.PTree -> Ast
fromPTreeToAst (P.Identifier str) = Iden str
fromPTreeToAst (P.Application t1 t2) =
  App (fromPTreeToAst t1) (fromPTreeToAst t2)
fromPTreeToAst (P.IfThenElse t1 t2 t3) =
  If (fromPTreeToAst t1) (fromPTreeToAst t2) (fromPTreeToAst t3)
fromPTreeToAst (P.Literal c) = Lit c
fromPTreeToAst (P.Binary bop t1 t2) =
  App (App (Iden $ fromBinOpToStr bop) (fromPTreeToAst t1)) $ fromPTreeToAst t2
fromPTreeToAst (P.Unary uop t) =
  App (Iden $ fromUnOpToStr uop) (fromPTreeToAst t)










-- fromBinaryOp to String
fromBinOpToStr :: P.BinOp -> String
fromBinOpToStr P.Plus          = "+"
fromBinOpToStr P.Minus         = "-"
fromBinOpToStr P.Multi         = "*"
fromBinOpToStr P.Div           = "/"
fromBinOpToStr P.Mod           = "%"
fromBinOpToStr P.LessThan      = "<"
fromBinOpToStr P.LessThanEq    = "<="
fromBinOpToStr P.GreaterThan   = ">"
fromBinOpToStr P.GreaterThanEq = ">="
fromBinOpToStr P.Equal         = "="
fromBinOpToStr P.NotEqual      = "<>"
fromBinOpToStr P.Or            = "or"
fromBinOpToStr P.And           = "and"

fromUnOpToStr :: P.UnOp -> String
fromUnOpToStr P.Not = "not"
fromUnOpToStr P.Negate = "-"
