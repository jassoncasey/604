module Steve.Internal where

{-
  This file holds all common internal information.

  Included:
    PTree (Parse tree) definitions
    TTerm (Typed Term definitions)
-}

import Data.List (isPrefixOf)

data Type' =
    SNat'
  | SChar'
  | SBool'
  | List' Type'
  | Func' Type' Type'
  | TVar' String
  | UserType' String
  | Array' Type' Ast
  | Uint' Ast Ast
  | Dep' Ast Type' Type'
  | Rec' [(String,Type')]
  | Rec1' RecordType
  deriving (Show,Eq)

-- Type for records
data RecordType =
    Record String Type' RecordType
  | EmptyRecord
  deriving (Show,Eq)


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
  | Dep Term Type Type
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
  | Pdu' Type'     -- Rename this to PduCtor
  | PduDef String [(String,Type')]
  | PduDefPart [(String,Type')]
  | Abs' String Type' Ast
  | App' Ast Ast
  | If' Ast Ast Ast
  | Let' String Type' Ast Ast
  | Case' Ast [(String,[TypeBinding],Ast)]
  | Proj' Ast String 
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
  | Projection PTree String
  deriving (Eq, Show)

-- FIXME And better support for userdata
data Constant = 
   LitBool Bool
 | LitNat  Integer
 | LitChar Char
 deriving (Show,Eq)
{-instance Show Constant where
  show c = case c of
    LitBool h -> show h
    LitNat e  -> show e
    LitChar y -> show y-}

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
  | NewPduDecl String [PduPField] 
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

getFieldName :: PduPField -> String
getFieldName (PPadF _) = "$nil"
getFieldName (PNormField str _) = str
getFieldName (PIfThenF str _ _) = str



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



-- TypeCheck Monad
{-============================================================================-}
-- I know either can be used, but I had to learn how to do it myself
data TypeChecker a = Good a | Bad String deriving (Show,Eq)

instance Monad TypeChecker where
  return x = Good x
  Good x >>= f = f x
  Bad msg >>= f = Bad msg
  fail msg = Bad msg

-- Take a list of type checked things and return a typechecked list.
-- If any element is Bad, return bad. Otherwise, return a list of Good
fromTypeCheckList :: [TypeChecker a] -> TypeChecker [a]
fromTypeCheckList [] = Good []
fromTypeCheckList ((Good x):xs) = fromTypeCheckList xs>>= \xs' -> return (x:xs')
fromTypeCheckList ((Bad msg):_) = Bad msg

mapTC :: (a -> TypeChecker b) -> [a] -> TypeChecker [b]
mapTC f xs = fromTypeCheckList $ map f xs



-- This is the generalized environment. It takes kind and type bindings
data Gamma =
    TypeBind String Type'
  | KindBind Type' Kind
  deriving (Eq)
instance Show Gamma where
  show x = case x of
    TypeBind n t -> show "(" ++ n ++ "," ++ show t ++ ")"
    KindBind t k -> show "(" ++ show t ++ "::" ++ show k ++ ")"

type Sigma = (String,Type')

kindLookup :: Type' -> [Gamma] -> TypeChecker Kind
kindLookup t [] = Bad ("There is no kind for type " ++ show t)
kindLookup t (b:bs) = case b of
  KindBind t' k | t == t' -> Good k
  _                       -> kindLookup t bs

typeLookup :: String -> [Gamma] -> TypeChecker Type'
typeLookup n [] = Bad ("Variable '" ++ show n ++ "' not in scope.")
typeLookup n (b:bs) = case b of
  TypeBind n' t | n == n' -> Good t
  _                       -> typeLookup n bs

ensure :: Bool -> String -> TypeChecker Type'
ensure True _ = Good SNat'
ensure False msg = Bad msg

sigmaLookup :: String -> [Sigma] -> TypeChecker Type'
sigmaLookup n sigma = case lookup n sigma of
  Just typ -> Good typ
  Nothing  -> Bad ("'" ++ n ++ "' not in Sigma.")

rhoLookup :: String -> Type' -> TypeChecker Type'
rhoLookup n (Rec' recs) = case lookup n recs of
  Just typ -> Good typ
  Nothing  -> Bad ("'" ++ n ++ "' not in pho." ++ show recs)


typeOfConstant :: Constant -> Type'
typeOfConstant (LitBool _) = SBool'
typeOfConstant (LitNat  _) = SNat'
typeOfConstant (LitChar _) = SChar'

-- Get input and output types of a Func
typeOfFuncInM :: Type' -> Maybe Type'
typeOfFuncInM (Func' typ _) = Just typ
typeOfFuncInM _ = Nothing
typeOfFuncOutM :: Type' -> Maybe Type'
typeOfFuncOutM (Func' _ typ) = Just typ
typeOfFuncOutM _ = Nothing

typeOfArg :: Type' -> TypeChecker Type'
typeOfArg (Func' typ _) = Good typ
typeOfArg _ = Bad "Not a function type."
typeOfOut :: Type' -> TypeChecker Type'
typeOfOut (Func' _ typ) = Good typ
typeOfOut _ = Bad "Not a function type."

-- errors
checkSameType :: Type' -> Type' -> String -> TypeChecker Type'
checkSameType t u str = if t == u
  then return SNat'
  else Bad (str ++ " Couldn't match expected type '"++ show t
           ++ "' with type '" ++ show u ++ "'.")

checkSameList :: Type' -> [Type'] -> String -> TypeChecker Type'
checkSameList t ts str = if all (== t) ts
  then return SNat'
  else Bad (str ++ "Couldn't match type " ++ show t ++ " with the types "
    ++ show ts ++ ".")

notInSigma :: String -> [Sigma] -> TypeChecker Type'
notInSigma x sigma = case lookup x sigma of
  Just a -> Bad ("Ambiguous occurance of" ++ x)
  _ -> Good SNat'

notIn :: (Eq a, Show a) => a -> [(a,b)] -> TypeChecker Type'
notIn x xs = case lookup x xs of
  Just a -> Bad ("Ambiguous occurance of" ++ show x)
  _ -> Good SNat'


betaReduceType :: String -> String -> Type' -> Type'
betaReduceType from to SNat' = SNat'
betaReduceType from to SChar' = SChar'
betaReduceType from to SBool' = SBool'
betaReduceType from to (List' t) = betaReduceType from to t
betaReduceType from to (Func' t1 t2) = Func' t1' t2'
  where t1' = betaReduceType from to t1
        t2' = betaReduceType from to t2
betaReduceType from to (Rec' fs) = Rec' $ map betaReduceRecord fs
  where betaReduceRecord :: (String,Type') -> (String,Type')
        betaReduceRecord (n,t) = if n == from
          then (to  ,betaReduceType from to t)
          else (from,betaReduceType from to t)
betaReduceType from to (Dep' e t1 t2) = Dep' e' t1' t2'
  where e'  = betaReduceTerm from to e
        t1' = betaReduceType from to t1
        t2' = betaReduceType from to t2
betaReduceType _    _  (TVar' n) = TVar' n 
betaReduceType from to (UserType' n) = UserType' n
betaReduceType from to (Uint' e1 e2) = Uint' e1' e2'
  where e1' = betaReduceTerm from to e1
        e2' = betaReduceTerm from to e2


betaReduceTerm :: String -> String -> Ast -> Ast
betaReduceTerm from to (Iden' n) = if from == n then (Iden' to) else (Iden' n)
betaReduceTerm _    _  lit@(Lit' _) = lit
betaReduceTerm from to (Abs' arg t e) = if from == arg
  then (Abs' to t' e')
  else (Abs' from t' e')
  where e' = betaReduceTerm from to e
        t' = betaReduceType from to t
betaReduceTerm from to (App' e1 e2) = App' e1' e2'
  where e1' = betaReduceTerm from to e1
        e2' = betaReduceTerm from to e2


-- finds rhos and replaces them with 
rhoReplaceType :: Ast -> Type' -> Type'
rhoReplaceType e SNat' = SNat'
rhoReplaceType e SChar' = SChar'
rhoReplaceType e SBool' = SBool'
rhoReplaceType e (List' t) = rhoReplaceType e t
rhoReplaceType e (Func' t1 t2) = Func' t1' t2'
  where t1' = rhoReplaceType e t1
        t2' = rhoReplaceType e t2
{-rhoReplaceType e (Rec' fs) = Rec' $ map rhoReplaceRecord fs
  where rhoReplaceRecord :: (String,Type') -> (String,Type')
        rhoReplaceRecord (n,t) = if n == from
          then (to  ,rhoReplaceType from to t)
          else (from,rhoReplaceType from to t)-}
rhoReplaceType e (Dep' e' t1 t2) = Dep' e'' t1' t2'
  where e'' = rhoReplaceTerm e e'
        t1' = rhoReplaceType e t1
        t2' = rhoReplaceType e t2
rhoReplaceType _ (TVar' n) = TVar' n 
rhoReplaceType e (UserType' n) = UserType' n
rhoReplaceType e (Uint' e1 e2) = Uint' e1' e2'
  where e1' = rhoReplaceTerm e e1
        e2' = rhoReplaceTerm e e2

rhoReplaceTerm :: Ast -> Ast -> Ast
rhoReplaceTerm e (Iden' n) =
  if isPrefixOf "$rho." n
    then Proj' e (drop 4 n)
    else (Iden' n)
rhoReplaceTerm _ lit@(Lit' _) = lit
rhoReplaceTerm e (Abs' arg t e') = (Abs' arg t' e'')
  where e'' = rhoReplaceTerm e e'
        t' = rhoReplaceType e t
rhoReplaceTerm e (App' e1 e2) = App' e1' e2'
  where e1' = rhoReplaceTerm e e1
        e2' = rhoReplaceTerm e e2