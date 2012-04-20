module Steve.Internal where

{-
  This file holds all common internal information.

  Included:
    PTree (Parse tree) definitions
    TTerm (Typed Term definitions)
-}

data Type =
    SNat
  | SBool
  | SChar
  | TVar String
  | Pdu String
  | Adt String
  | List Type
  | Func Type Type
  deriving (Show,Eq)


data PTree =
    Identifier String
  | Application PTree PTree
  | IfThenElse PTree PTree PTree
  | Binary BinOp PTree PTree
  | Unary UnOp PTree
  | Literal Constant
  deriving (Eq, Show)

data Constant = 
   LitBool Bool
 | LitNat  Integer
 | LitChar Char
 deriving (Eq, Show)

data BinOp =
    Plus | Minus | Multi | Div | Mod                    -- Arithmetic
  | LessThan | LessThanEq | GreaterThan | GreaterThanEq -- Ordering
  | Equal | NotEqual                                    -- Equality
  | Or | And                                            -- Logical
  deriving (Eq, Show)

data UnOp = Not | Negate deriving (Eq, Show)

type TopLevelFunc = (String, Type, [String], PTree)

-- Terms in Steve are fully typed. There is no inference
data Term =
    Iden String
  | Lit Constant
  | Abs String Type Term
  | App Term Term
  | If Term Term Term
  | Let String Type Term Type Term
  deriving (Eq,Show)

