module Typing where

-- FIXME \x.(let y = \z.(cons z x); y 3; y 3); doesn't work

import Ast
import Data.List( nub )
import Data.Maybe( fromJust )

data TypeSymbol =
    Arrow TypeSymbol TypeSymbol
  | List TypeSymbol
  | Natural
  | TypeVar Int
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
decltype :: Ast.Ast -> TypeSymbol
decltype ast = let (_,t,_) = declType' [] 0 ast in t

declType' :: TypeContext -> Int -> Ast.Ast -> (Int,TypeSymbol,ConstraintSet)

-- TAUT' rule
declType' ctx n (Ast.Variable (Ast.Identifier sym)) =
  case ty of
    Just ty' -> (n,ty',[])
    Nothing -> error ("Lookup failure:'" ++ sym ++ "' is not defined.")
  where ty = lookUp ctx sym
-- This should never happen...
declType' _ _ (Ast.Variable (Ast.Unique _)) = error "Found a unique variable."

-- Primitives follow 'delta' rules. So
--   '+','-','*','/' :: Natural -> Natural -> Natural
declType' _ n (Ast.Constant (Ast.Primitive sym _))
  | elem sym ["+","-","*","/"] = (n,(Arrow Natural (Arrow Natural Natural)),[])
  | otherwise = error ("Lookup failure:'" ++ sym ++ "' is not defined.")

-- Constructor types defined in Assignment 3
--   cons :: forall a.a -> [a] -> [a]
--   nil :: forall a.[a]
declType' _ n (Ast.Constant (Ast.Constructor "cons" _)) =
  let t = TypeVar n in
  (n + 1 ,Arrow t (Arrow (List t) (List t)),[])
declType' _ n (Ast.Constant (Ast.Constructor "nil" _)) =
  (n + 1 , List (TypeVar n),[])
declType' _ _ (Ast.Constant (Ast.Constructor a _)) =
  error ("Unknown constructor " ++ a ++ ".")

-- IntCst are type nat
declType' _ n (Ast.Constant (Ast.IntCst _)) = (n,Natural,[])

-- ABS rule
declType' ctx n (Ast.Lambda x e) =
  let
    ctx' = bindVar ctx x (TypeVar n)
    (n',te,cs) = declType' ctx' (n+1) e
    te' = unifyOn cs te
  in case te' of
    Just c -> (n',Arrow (TypeVar n) c,cs)
    Nothing -> error ("Couldn't unify")

-- APP rule
-- FIXME Can't we discard some assumptions? How do constraints found previously
--       help us?
declType' ctx n (Ast.Application e1 e2) =
  case atob of
    Arrow _ b' -> let constr = (atob,Arrow a b') in
      case unified of
        Just c -> (n'', c, [(atob,Arrow a b')])
        Nothing -> error ("Expected a', but got a")
      where unified = unifyOn ((atob,Arrow a b'):(c1++c2)) b'
    _ -> error "Cannot apply non-lambda 'atob' to 'a'"
  where (n',atob,c1)  = declType' ctx n e1  -- get constraints and type from lhs
        (n'',a,c2) = declType' ctx n' e2    -- get constraints and type from rhs

-- When a let expression occurs with a unique id, then the variable is not used
-- elsewhere in the spl program. As such we only run decltype on the expression
-- to ensure type safety. It's type information is not relevant to the rest of
-- the program.
declType' ctx n (Ast.Let (Ast.Unique _) e1 e2) =
  let _ = declType' ctx 0 e1 in declType' ctx n e2

-- LET' rule
declType' ctx n (Ast.Let (Ast.Identifier sym) e1 e2) =
  let
    (n',e1t,cs1) = declType' ctx n e1
    ctx' = bindVar ctx sym e1t
    (n'',e2t,cs2) = declType' ctx' n' e2
    unified = unifyOn (cs1++cs2) e2t
  in case unified of
    Just c -> (n'',c,cs1++cs2)
    Nothing -> error "Unification Error!"



-- Given a set of type constraints, unify returns a principle unifier
-- FIXME change this to return Either. The left will return an error message
{-unify :: ConstraintSet -> Substitution
unify [] = []
unify ((t,u):cs)
  | t == u = unify cs
  | isTypeVar t && (not $ isSubVar t u) = (t,u) : unify (subConstraints t u cs)
  | isTypeVar u && (not $ isSubVar u t) = (u,t) : unify (subConstraints u t cs)
  | isArrow t && isArrow u =
      let
        (Arrow t1 t2) = t
        (Arrow u1 u2) = u
      in unify ((t1,u1):(t2,u2):cs)
  | isList t && isList u =
      let
        (List t') = t
        (List u') = u
      in unify ((t',u'):cs)
  | otherwise = error "SUUUUUUPP, HOLMES!"-}

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