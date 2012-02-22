module Typing where

import qualified Ast
--import qualified Env

data TypeSymbol =
    Arrow TypeSymbol TypeSymbol
  | List TypeSymbol
  | Natural
  | Any
  | TypeVar Int
  deriving (Show,Eq)

data AstT = 
    Constant TypeSymbol Ast.CstData
  | Variable TypeSymbol Ast.Name
  | Lambda TypeSymbol String AstT
  | Application TypeSymbol AstT AstT
  | Let TypeSymbol AstT AstT
  deriving (Show,Eq)

-- typing environment
-- FIXME: Move to own file?
type TypeContext = [(Ast.Name,TypeSymbol)]

type ConstraintSet = [(TypeSymbol,TypeSymbol)]

-- Differs from above as it models substituion function.
-- Given any tuple, the first element is a Variable type symbol
type Substitution = [(TypeSymbol,TypeSymbol)]



typifyTree :: Ast.Ast -> AstT
typifyTree t = let (t',_) = typifyTreeImpl' (t,0) in t'

typifyTreeImpl :: TypeContext -> Ast.Ast -> Int -> (AstT, Int)
typifyTreeImpl ctx (Ast.Constant (Ast.IntCst a)) n =
  (Constant Natural (Ast.IntCst a), n)
typifyTreeImpl ctx (Ast.Constant (Ast.Primitive sym k)) n
  | elem sym ["+","-","*","/"] =
      (Constant (Arrow Natural (Arrow Natural Natural)) (Ast.Primitive sym k),n)
  | otherwise = error "Unknown Primitive!"
typifyTreeImpl ctx (Ast.Variable (Ast.Identifier sym)) n =
  case typeVar of
    Just a -> ((Variable a (Ast.Identifier sym)),n)
    Nothing -> error ("Unbound variable '" ++ sym ++ "'.")
  where typeVar = lookUp ctx sym
{-typifyTreeImpl ctx (Ast.Lambda sym e) n =
  let
    ctx' = bindVar ctx sym (TypeVar n)
    (et,n') = typifyTreeImpl ctx' e
  in-}


typifyTreeImpl' :: (Ast.Ast, Int) -> (AstT, Int)
typifyTreeImpl' ((Ast.Constant (Ast.IntCst a)), n) =
  (Constant Natural (Ast.IntCst a), n)
typifyTreeImpl' ((Ast.Constant (Ast.Primitive sym k)), n)
  | elem sym ["+","-","*","/"] =
      (Constant (Arrow Natural (Arrow Natural Natural)) (Ast.Primitive sym k),n)
  | otherwise = error "Unknown Primitive!"
typifyTreeImpl' ((Ast.Variable name), n) =
  ((Variable (TypeVar n) name), n+1)
typifyTreeImpl' ((Ast.Lambda sym e), n) =
  let (e',n') = typifyTreeImpl' (e,n) in
  ((Lambda (TypeVar n') sym e'), n' + 1)
typifyTreeImpl' ((Ast.Application e1 e2), n) =
  let
    (e1',n')  = typifyTreeImpl' (e1,n)
    (e2',n'') = typifyTreeImpl' (e2,n')
  in
    ((Application (TypeVar n'') e1' e2'), n'' + 1)



lookUp :: TypeContext -> String -> Maybe TypeSymbol
lookUp (((Ast.Identifier nextSym),ty):cs) sym
  | sym == nextSym = Just ty
  | otherwise = lookUp cs sym
lookUp (((Ast.Unique _),ty):cs) sym = lookUp cs sym
lookUp _ _ = Nothing

bindVar :: TypeContext -> String -> TypeSymbol -> TypeContext
bindVar context sym ty = ((Ast.Identifier sym),ty):context


-- Infertype does not use unify. Instead it applies the rules as needed.
-- Should probably not do that...
inferType :: TypeContext -> AstT -> TypeSymbol
{-inferType ctx (Application t e1 e2)
  | isArrow ty1 = let (Arrow ty11 ty12) = ty1 in
    case ty11 of
      ty2 -> ty12
      _   -> error "Expected, got m!y"
  | otherwise = error "Expected an arrow type."
  where ty1 = inferType ctx e1
        ty2 = inferType ctx e2-}

{-inferType ctx (Ast.Lambda sym e)
  let
    ctx' = bindVar ctx sym -}

inferType _ (Constant ty (Ast.IntCst _)) = ty

inferType _ (Constant ty (Ast.Primitive _ _)) = ty

inferType ctx (Variable _ (Ast.Identifier sym)) =
  case ty of
    Just ty' -> ty'
    Nothing -> error ("Lookup failure. " ++ sym ++ " is not defined.")
  where ty = lookUp ctx sym


-- decltype finds the list straight from the Ast
-- Assume that the context is populated?
declType :: TypeContext -> Int -> Ast.Ast -> (Int,TypeSymbol)
declType ctx n (Ast.Variable (Ast.Identifier sym)) =
  case ty of
    Just ty' -> (n,ty')
    Nothing -> error ("Lookup failure:'" ++ sym ++ "' is not defined.")
  where ty = lookUp ctx sym
declType _ n (Ast.Constant (Ast.Primitive sym _))
  | elem sym ["+","-","*","/"] = (n,(Arrow Natural (Arrow Natural Natural)))
  | otherwise = error ("Lookup failure:'" ++ sym ++ "' is not defined.")

declType _ n (Ast.Constant (Ast.Constructor "cons" _)) =
  let t = TypeVar n in
  (n + 1 ,Arrow t (Arrow (List t) (List t)))
declType _ n (Ast.Constant (Ast.Constructor "nil" _)) =
  (n + 1 , List (TypeVar n))
declType _ _ (Ast.Constant (Ast.Constructor a _)) =
  error ("Unknown constructor " ++ a ++ ".")

declType _ n (Ast.Constant (Ast.IntCst _)) = (n,Natural)

declType ctx n (Ast.Lambda sym e) =
  let
    ctx' = bindVar ctx sym (TypeVar n)
    (n', te) = declType ctx' (n+1) e
  in (n', Arrow (TypeVar n) te)

declType ctx n (Ast.Application e1 e2)
  | isArrow atob = case atob of
      Arrow a' b -> (n'',subType a' a b)
      _ -> error ("Expected a, but got _ in _")
  | otherwise = error ("Expected a -> b, but got _ in _")
  where (n',atob)  = declType ctx n e1
        (n'',a) = declType ctx n' e2

-- let statements with unique names are thrown away. Hence, the type result is
--declType ctx n (Ast.Let (Unique _) e1 e2)
    

-- This decltype gathers constraints on application
declType' :: TypeContext -> Int -> Ast.Ast -> (Int,TypeSymbol,ConstraintSet)
declType' ctx n (Ast.Variable (Ast.Identifier sym)) =
  case ty of
    Just ty' -> (n,ty',[])
    Nothing -> error ("Lookup failure:'" ++ sym ++ "' is not defined.")
  where ty = lookUp ctx sym
declType' _ n (Ast.Constant (Ast.Primitive sym _))
  | elem sym ["+","-","*","/"] = (n,(Arrow Natural (Arrow Natural Natural)),[])
  | otherwise = error ("Lookup failure:'" ++ sym ++ "' is not defined.")

declType' _ n (Ast.Constant (Ast.Constructor "cons" _)) =
  let t = TypeVar n in
  (n + 1 ,Arrow t (Arrow (List t) (List t)),[])
declType' _ n (Ast.Constant (Ast.Constructor "nil" _)) =
  (n + 1 , List (TypeVar n),[])
declType' _ _ (Ast.Constant (Ast.Constructor a _)) =
  error ("Unknown constructor " ++ a ++ ".")

declType' _ n (Ast.Constant (Ast.IntCst _)) = (n,Natural,[])

declType' ctx n (Ast.Lambda x e) =
  let
    ctx' = bindVar ctx x (TypeVar n)
    (n',te,cs) = declType' ctx' (n+1) e
    te' = unifyOn cs te
  in (n',Arrow (TypeVar n) te',cs)

declType' ctx n (Ast.Application e1 e2)
  | isArrow atob = case atob of
      Arrow a' b -> (n'',b , ((atob,Arrow a b):(c1++c2)))
      _ -> error ("Expected a, but got _ in _")
  | otherwise = error ("Expected a -> b, but got _ in _")
  where (n',atob,c1)  = declType' ctx n e1
        (n'',a,c2) = declType' ctx n' e2

declType' ctx n (Ast.Let (Ast.Unique _) e1 e2) =
  let _ = declType' ctx 0 e1 in declType' ctx n e2

declType' ctx n (Ast.Let (Ast.Identifier sym) e1 e2) =
  let
    (n',e1t,cs1) = declType' ctx n e1
    ctx' = bindVar ctx sym e1t
    (n'',e2t,cs2) = declType' ctx' n' e2
  in (n'',unifyOn (cs1++cs2) e2t,cs1++cs2)


-- Gathers all constraints for an Ast
getType :: AstT -> TypeSymbol
getType (Constant t _) = t
getType (Variable t _) = t
getType (Lambda t _ _) = t
getType (Application t _ _) = t
getType (Let t _ _) = t

getConstraints :: AstT -> ConstraintSet -> ConstraintSet
getConstraints (Application t e1 e2) cs =
  let
    cs1 = getConstraints e1 []
    cs2 = getConstraints e2 []
    constraint = (getType e1, Arrow (getType e2) t)
  in constraint : (cs1++cs2++cs)
getConstraints (Constant _ _) cs = cs
getConstraints (Variable _ _) cs = cs
getConstraints (Lambda _ _ e) cs = getConstraints e cs



-- Given a set of type constraints, unify returns a principle unifier (substituion)
-- FIXME change this to return Either. The left will return an error message
unify :: ConstraintSet -> Substitution
unify [] = []
unify ((t,u):cs)
  | t == u = unify cs
  -- The book appends the new substitution to the back
  | isTypeVar t && (not $ isSubVar t u)
      = (t,u) : unify (subConstraints t u cs)
  | isTypeVar u && (not $ isSubVar u t)
      = (u,t) : unify (subConstraints u t cs)
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
  | otherwise = error "SUUUUUUPP, HOLMES!"

unifyOn :: ConstraintSet -> TypeSymbol -> TypeSymbol
unifyOn [] t = t
unifyOn cs t = applySub (unify cs) t

isTypeVar :: TypeSymbol -> Bool
isTypeVar (TypeVar _) = True
isTypeVar _ = False

isArrow :: TypeSymbol -> Bool
isArrow (Arrow _ _) = True
isArrow _ = False

isList :: TypeSymbol -> Bool
isList (List _) = True
isList _ = False

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
subConstraints t u ((s,r):cs) = (subType t u s, subType t u r):(subConstraints t u cs)
subConstraints t u [] = []

-- Applies the substitution until the
applySub :: Substitution -> TypeSymbol -> TypeSymbol
applySub [] t = t
applySub ((s,u):subs) t = subType s u $ applySub subs t

--applySubOnce :: Substitution -> TypeSymbol -> TypeSymbol
--applySubOnce [] t = t
--applySubOnce ((s,u):subs) t = -}