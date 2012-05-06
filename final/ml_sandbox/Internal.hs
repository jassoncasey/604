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
  | Uint Term Term
  deriving (Eq)
instance Show Type where
  show typ = case typ of
    SNat -> "Nat"
    SChar -> "Char"
    SBool -> "Bool"
    List t -> "[" ++ show t ++ "]"
    Func t1 t2 -> "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    TVar str -> str
    UserType str -> str
    Array t e -> "Array " ++ show t ++ " (" ++ show e ++ ")"
    Uint e1 e2 -> "Uint (" ++ show e1 ++ ") (" ++ show e2 ++ ")"

-- Kinds of things
data Kind =
    Star
  | KApp Kind Kind
  | To Kind Kind
  | KType Type
  | KTerm Ast
  deriving (Eq)
instance Show Kind where
  show x = case x of
    Star -> "*"
    KApp k1 k2 -> "(" ++ show k1 ++ ") (" ++ show k2 ++ ")"
    KTerm e -> show e
    KType t -> show t

data Ast =
    Iden' String
  | Lit' Constant
  | Pdu' Type     -- Rename this to PduCtor
  | PduDef String [(String,Type)]
  | Abs' String Type Ast
  | App' Ast Ast
  | If' Ast Ast Ast
  | Let' String Type Ast Ast
  | Case' Ast [(String,[TypeBinding],Ast)]
  | Nil
  deriving (Show,Eq)


-- FIXME Make a prettier App rule
data Term =
    Iden String
  | Lit Constant
  | Pdu Type     -- Rename this to PduCtor
  | Abs String Type Term
  | App Term Term
  | If Term Term Term
  | Let String Type Term Term
  | Case Term [(String,[TypeBinding],Term)]
  deriving (Eq)
instance Show Term where
  show expr = case expr of
    Iden str   -> str
    Lit c      -> show c
    Pdu t      -> show t ++ " = {...}"
    Abs x t e  -> "\\" ++ x ++ " : " ++ show t ++ "." ++ show e
    App e1 e2  -> "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    If b e1 e2 -> "if " ++ show b ++ " then " ++ show e1 ++ " else " ++ show e2
    Let x t e prgm -> "let " ++ x ++ " = " ++ show e ++ " : " ++ show t
      ++ " in " ++ show prgm
    Case e _   -> "case " ++ show e ++ " of ..."

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
  | PUint PTree PTree
  | PPad PTree
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

data BinOp =
    Plus | Minus | Multi | Div | Mod                    -- Arithmetic
  | LessThan | LessThanEq | GreaterThan | GreaterThanEq -- Ordering
  | Equal | NotEqual                                    -- Equality
  | Or | And                                            -- Logical
  | BitAnd | BitOr | BitXor                             -- Bit Operations
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

-- These are the types that a pdu field can be
data PduPField =
    PPadF PTree
  | PNormField String PType
  | PIfThenF String PTree PType
  deriving (Show,Eq)



type Fact = Term

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