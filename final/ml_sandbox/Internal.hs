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
  | BitAnd | BitOr | BitXor                    -- Bit Operations
  deriving (Eq)
instance Show BinOp where
  show op = case op of
    Plus          -> "+"
    Minus         -> "-"
    Multi         -> "*"
    Div           -> "/"
    Mod           -> "%"
    LessThan      -> "<"
    LessThanEq    -> "<="
    GreaterThan   -> ">"
    GreaterThanEq -> ">="
    Equal         -> "=="
    NotEqual      -> "<>"
    Or            -> "or"
    And           -> "and"
    BitAnd        -> "&"
    BitOr         -> "|"
    BitXor        -> "^"

data UnOp = Not | BitNot | Negate deriving (Eq)
instance Show UnOp where
  show op = case op of
    Not    -> "not"
    BitNot -> "~"
    Negate -> "neg"


-- FIXME Redo parse info hierarchy. A pdu declaration should be distinct from an
--       ADT declaration


data Declaration =
    PduDecl PduInfo
  | AdtDecl AdtInfo
  | FuncDecl TopLevelFunc
  deriving (Show,Eq)

type PDURecord = (String, [(String,PType)])

data PduInfo = PduInfo {
  pduInfoName   :: String,
  pduInfoFields :: [(String,PType)]
} deriving (Show,Eq)

data AdtInfo = AdtInfo {
  adtInfoName  :: String,
  adtInfoCtors :: [(String,[PType])]
} deriving (Show,Eq)

data Declarations = Declarations {
  pduDecls  :: [PduInfo],
  adtDecls  :: [AdtInfo],
  funcDecls :: [TopLevelFunc]
}

-- name, type, param names, parse tree (definition)
type TopLevelFunc = (String, PType, [String], PTree)




type TypeBinding = (String, Type)


data Env = Env {
  gamma :: [TypeBinding]
}

bindType :: TypeBinding -> Env -> Env
bindType typeBinding (Env typeEnv) = Env (typeBinding : typeEnv)

bindTypes :: [TypeBinding] -> Env -> Env
bindTypes typeBindings (Env typeEnv) = Env (typeBindings ++ typeEnv)


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