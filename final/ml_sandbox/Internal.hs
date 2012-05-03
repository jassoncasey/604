module Steve.Internal where

{-
  This file holds all common internal information.

  Included:
    PTree (Parse tree) definitions
    TTerm (Typed Term definitions)
-}


-- FIXME Needs a pretty show instance
data Type =
    SNat
  | SChar
  | SBool
  | List Type
  | Func Type Type
  | TVar String
  | UserType String
  | Array Type Term
  | ArrayPartial Type
  | Uint Term Term
  | UintPartial Term
  deriving (Show,Eq)

-- FIXME Needs a pretty show instance
data Term =
    Iden String
  | Lit Constant
  | Pdu Type
  | Abs String Type Term
  | App Term Term
  | If Term Term Term
  | Let String Type Term Term
  | Case Term [(String,[TypeBinding],Term)]
  deriving (Eq,Show)

-- PType is a type whose terms contain parse trees, not terms
data PType =
    PSNat
  | PSChar
  | PSBool
  | PList PType
  | PFunc PType PType
  | PTVar String
  | PUserType String
  | PArray PType PTree
  | PArrayPartial PType
  | PUint PTree PTree
  | PUintPartial PTree
  deriving (Show,Eq)


data PTree =
    Identifier String
  | Application PTree PTree
  | IfThenElse PTree PTree PTree
  | Binary BinOp PTree PTree
  | Unary UnOp PTree
  | Literal Constant
  | CaseStmt PTree [(String,[String],PTree)] -- ctor name, param list, expr
  | PduConstructor String
  deriving (Eq, Show)

-- FIXME Needs a pretty show instance
-- FIXME And better support for userdata
data Constant = 
   LitBool Bool
 | LitNat  Integer
 | LitChar Char
 deriving (Eq)

instance Show Constant where
  show c = case c of
    LitBool h -> show h
    LitNat e  -> show e
    LitChar y -> show y

-- FIXME Needs a pretty show instance
data BinOp =
    Plus | Minus | Multi | Div | Mod                    -- Arithmetic
  | LessThan | LessThanEq | GreaterThan | GreaterThanEq -- Ordering
  | Equal | NotEqual                                    -- Equality
  | Or | And                                            -- Logical
  deriving (Eq, Show)

data UnOp = Not | Negate deriving (Eq, Show)


-- FIXME Redo parse info hierarchy. A pdu declaration should be distinct from an
--       ADT declaration


-- Types used to wrap top level declarations
data UserDataStructure =
    PDUType [(String,PType)]       -- A name and a list of fields
  | ADTType [(String,[PType])] -- A name and a list of constructors
  deriving (Eq, Show)

type UserTypeDef = (String, UserDataStructure)

data Declaration =
    TypeDecl UserTypeDef
  | AdtDecl AdtInfo
  | PduDecl PduInfo
  | FuncDecl TopLevelFunc
  deriving (Show,Eq)

type PDURecord = (String, [(String,PType)])

data PduInfo = PduInfo {
  pduInfoName :: String,
  pduInfoFields :: [(String,PType)]
} deriving (Show,Eq)

-- name, type, param names, parse tree (definition)
type TopLevelFunc = (String, PType, [String], PTree)




type TypeBinding = (String, Type)


-- Really common or really basic helper functions

lunzip :: [(a,b)] -> [a]
lunzip [] = []
lunzip [(x,_)] = [x]
lunzip ((x,_):xys) = x : (lunzip xys)

runzip :: [(a,b)] -> [b]
runzip [] = []
runzip [(_,y)] = [y]
runzip ((_,y):xys) = y : (runzip xys)

-- Gets [Maybe a] and produces Maybe [a]. Nothing if at least one element is
-- Nothing.
safeMaybeToList :: [Maybe a] -> Maybe [a]
safeMaybeToList [] = Just []
safeMaybeToList ((Just x):xs) = safeMaybeToList xs >>= \xs' -> return (x:xs')
safeMaybeToList ((Nothing):_) = Nothing

-- Checks to see if a list is unique
unique :: (Eq a) => [a] -> Bool
unique l = unique' l []
  where
    unique' [] _ = True
    unique' (x:xs) ls
      | elem x ls = False
      | otherwise = unique' xs (x:ls)

-- fromBinaryOp to String
fromBinOpToStr :: BinOp -> String
fromBinOpToStr Plus          = "+"
fromBinOpToStr Minus         = "-"
fromBinOpToStr Multi         = "*"
fromBinOpToStr Div           = "/"
fromBinOpToStr Mod           = "%"
fromBinOpToStr LessThan      = "<"
fromBinOpToStr LessThanEq    = "<="
fromBinOpToStr GreaterThan   = ">"
fromBinOpToStr GreaterThanEq = ">="
fromBinOpToStr Equal         = "="
fromBinOpToStr NotEqual      = "<>"
fromBinOpToStr Or            = "or"
fromBinOpToStr And           = "and"

fromUnOpToStr :: UnOp -> String
fromUnOpToStr Not = "not"
fromUnOpToStr Negate = "-"