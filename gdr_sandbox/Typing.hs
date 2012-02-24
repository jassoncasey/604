module Typing where

-- FIXME \x.(let y = \z.(cons z x); y 3; y 3); doesn't work

import Ast
import Data.List( nub )
import Data.Maybe( fromJust )

import qualified Evaluate

data TypeSymbol =
    Arrow TypeSymbol TypeSymbol
  | List TypeSymbol
  | Natural
  | TypeVar Int
--  | ForAll TypeSymbol
  deriving (Show,Eq)

-- typing environment
type TypeContext = [(Ast.Name,TypeSymbol)]

type ConstraintSet = [(TypeSymbol,TypeSymbol)]

-- Differs from above as it models substituion function.
-- Given any tuple, the first element is a Variable type symbol
type Substitution = [(TypeSymbol,TypeSymbol)]

-- FIXME Replace with generic
lookUp :: TypeContext -> String -> Maybe TypeSymbol
lookUp (((Ast.Identifier nextSym),ty):cs) sym
  | sym == nextSym = Just ty
  | otherwise = lookUp cs sym
lookUp (((Ast.Unique _),_):cs) sym = lookUp cs sym
lookUp _ _ = Nothing

bindVar :: TypeContext -> String -> TypeSymbol -> TypeContext
bindVar context sym ty = ((Ast.Identifier sym),ty):context



-- This declType' gathers constraints on application
-- FIXME - discard old constraints?
-- FIXME - Turn Decltype into a giant case statement?
-- FIXME - return type should be maybe
decltype :: Ast.Ast -> Maybe TypeSymbol
--decltype ast = let (_,t,_) = declType' [] 0 ast in t
decltype ast =
  let
    (_,t,subs) = decltype' [] [] 0 ast
    (t',_) = unifyOn' subs t
  in t'


-- old LET' rule
{-declType' ctx n (Ast.Let (Ast.Identifier sym) e1 e2) =
  let
    (n',e1t,cs1) = declType' ctx n e1
    ctx' = bindVar ctx sym e1t
    (n'',e2t,cs2) = declType' ctx' n' e2
    unified = unifyOn (cs1++cs2) e2t
  in case unified of
    Just c -> (n'',c,cs1++cs2)
    Nothing -> error "Unification Error!"-}

------------------------
------------------------
------------------------
-- New and imporved declType
decltype' :: TypeContext -> Substitution -> Int -> Ast.Ast
             -> (Int,TypeSymbol,Substitution)
-- TAUT' rule
decltype' ctx _ n (Ast.Variable (Ast.Identifier sym)) =
  case ty of
    Just ty' -> (n,ty',[])
    Nothing -> error ("The variable '" ++ sym ++ "' is not defined.")
  where ty = lookUp ctx sym
-- This should never happen...
decltype' _ _ _ (Ast.Variable (Ast.Unique _)) = error "Found a unique variable."

-- Primitives follow 'delta' rules. So
--   '+','-','*','/' :: Natural -> Natural -> Natural
decltype' _ _ n (Ast.Constant (Ast.Primitive sym _))
  | elem sym ["+","-","*","/"] = (n,(Arrow Natural (Arrow Natural Natural)),[])
  | otherwise = error ("Lookup failure:'" ++ sym ++ "' is not defined.")

-- Constructor types defined in Assignment 3
--   cons :: forall a.a -> [a] -> [a]
--   nil :: forall a.[a]
decltype' _ _ n (Ast.Constant (Ast.Constructor "cons" _)) =
  let t = TypeVar n in
  (n + 1 ,Arrow t (Arrow (List t) (List t)),[])
decltype' _ _ n (Ast.Constant (Ast.Constructor "nil" _)) =
  (n + 1 , List (TypeVar n),[])
decltype' _ _ _ (Ast.Constant (Ast.Constructor a _)) =
  error ("Unknown constructor " ++ a ++ ".")

decltype' _ _ n (Ast.Constant (Ast.IntCst _)) = (n,Natural,[])

-- Lambda
decltype' ctx _ n (Ast.Lambda x e) =
  let
    newTypeVar = (TypeVar n)
    ctx' = bindVar ctx x newTypeVar
    (n', typeof_e, subs) = decltype' ctx' [] (n+1) e
  in (n', Arrow newTypeVar typeof_e,subs)

-- Application
decltype' ctx _ n (Ast.Application e1 e2) =
  case atob of
    Arrow _ b' -> case maybe_ty of
        Just ty -> (n'', ty, subs)
        Nothing -> error ("Expected type _, but got _ instead.")
      where (maybe_ty,subs) = unifyOn' ((atob, Arrow a b'):(subs1++subs2)) b'
    _ -> error "Expected a function type, but got 'atob' instead."
  where (n' ,atob,subs1) = decltype' ctx [] n e1
        (n'',a   ,subs2) = decltype' ctx [] n' e2

-- Let 
decltype' ctx _ n (Ast.Let (Ast.Unique _) e1 e2) =
  let _ = decltype' ctx [] 0 e1 in decltype' ctx [] n e2
-- Does this via betareduction. BADNESS! PLEASE CHANGE!!!
decltype' ctx _ n (Ast.Let (Ast.Identifier sym) e1 e2) =
  let e2' = Evaluate.betaReduction sym e1 e2
  in decltype' ctx [] n e2'
    --(n' ,ForAll typeof_e1,subs1) = decltype' ctx [] n e1
    --ctx' = bindVar ctx sym typeof_e1
    --(n'',typeof_e2,subs2) = decltype' ctx' [] n e1
  --in (n'',typeof_e2,subs1++subs2)


-- Need Soft fail with maybe!
unify' :: ConstraintSet -> Substitution
unify' [] = []
unify' ((t,u):cs) =
  case (t,u) of
    _ | t == u -> unify' cs
    ((TypeVar _),_) | (not $ isSubVar t u) ->
      (t,u): unify' (subConstraints t u cs)
    (_,(TypeVar _)) | (not $ isSubVar u t) ->
      (u,t) : unify' (subConstraints u t cs)
    (Arrow t1 t2,Arrow u1 u2) -> unify' ((t1,u1):(t2,u2):cs)
    (List t', List u') -> unify' ((t',u'):cs)
    _ -> error "SUUUUUUPP, HOLMES!"


unifyOn :: ConstraintSet -> TypeSymbol -> Maybe TypeSymbol
unifyOn [] t = Just $ t
unifyOn cs t = Just $ applySub (unify' cs) t
unifyOn' :: ConstraintSet -> TypeSymbol -> (Maybe TypeSymbol,Substitution)
unifyOn' [] t = (Just $ t, [])
unifyOn' cs t =
  let subs = unify' cs in (Just $ applySub subs t, subs)

isSubVar :: TypeSymbol -> TypeSymbol -> Bool
isSubVar t (List u) = t == u || isSubVar t u
isSubVar t (Arrow u s) = t == u || t == s || isSubVar t u || isSubVar t s
isSubVar _ _ = False

-- Given type t is equal to u, replace all instances of t in r with u
subType :: TypeSymbol -> TypeSymbol -> TypeSymbol -> TypeSymbol
subType t u (Arrow r s) = Arrow (subType t u r) (subType t u s)
subType t u (List r) = List (subType t u r)
subType t u r
  | t == r = u
  | otherwise = r

-- Substitute every occurance of t with u
subConstraints :: TypeSymbol -> TypeSymbol -> ConstraintSet -> ConstraintSet
subConstraints t u ((s,r):cs) =
  (subType t u s, subType t u r):(subConstraints t u cs)
subConstraints _ _ [] = []

-- Exhaustively applies the substitution to a type variable
applySub :: Substitution -> TypeSymbol -> TypeSymbol
applySub [] t = t
applySub ((s,u):subs) t = subType s u $ applySub subs t



-- pretty printing for TypeSymbol
printType :: TypeSymbol -> String
printType t =
  printTypeImpl t (zip (nub $ getTypeVars t) ['t':(show i) | i <- [(0::Int)..]])

printTypeImpl :: TypeSymbol -> [(Int,String)] -> String
printTypeImpl t typeMap =
  case t of
    Natural   -> "Nat"
    List u    -> "[" ++ (printTypeImpl u typeMap) ++ "]"
    TypeVar n -> fromJust $ lookup n typeMap
    Arrow u r -> case u of
      Arrow _ _ -> "(" ++ (printTypeImpl u typeMap) ++ ") -> "
                   ++ (printTypeImpl r typeMap)
      _ -> (printTypeImpl u typeMap) ++ " -> " ++ (printTypeImpl r typeMap)

getTypeVars :: TypeSymbol -> [Int]
getTypeVars (Arrow t u) = getTypeVars t ++ getTypeVars u
getTypeVars (List t) = getTypeVars t
getTypeVars (TypeVar n) = [n]
getTypeVars _ = []

isTypeVar :: TypeSymbol -> Bool
isTypeVar (TypeVar _) = True
isTypeVar _ = False

isArrow :: TypeSymbol -> Bool
isArrow (Arrow _ _) = True
isArrow _ = False

isList :: TypeSymbol -> Bool
isList (List _) = True
isList _ = False
