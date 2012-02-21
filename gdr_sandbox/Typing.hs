module Typing where

import qualified Ast


data TypeSymbol =
    Arrow TypeSymbol TypeSymbol
  | List TypeSymbol
  | Natural
  | Variable Int
  deriving (Show,Eq)

-- typing environment
-- FIXME: Move to own file?
type TypeContext = [(Ast.Name,TypeSymbol)]

type ConstraintSet = [(TypeSymbol,TypeSymbol)]

-- Differs from above as it models substituion function.
-- Given any tuple, the first element is a Variable type symbol
type Substitution = [(TypeSymbol,TypeSymbol)]

lookUp :: TypeContext -> String -> Maybe TypeSymbol
lookUp (((Ast.Identifier nextSym),ty):cs) sym
  | sym == nextSym = Just ty
  | otherwise = lookUp cs sym
lookUp (((Ast.Unique _),ty):cs) sym = lookUp cs sym
lookUp _ _ = Nothing

bindVar :: TypeContext -> String -> TypeSymbol -> TypeContext
bindVar context sym ty = ((Ast.Identifier sym),ty):context



inferType :: TypeContext -> Ast.Ast -> TypeSymbol
inferType context (Ast.Variable (Ast.Identifier sym)) =
  case ty of
    Just ty' -> ty'
    Nothing -> error ("Lookup failure. " ++ sym ++ " is a free variable.")
  where ty = lookUp context sym

inferType _ (Ast.Constant (Ast.IntCst _)) = Natural
inferType _ (Ast.Constant (Ast.Primitive sym _))
  | elem sym ["+","-","*","/"] = Arrow Natural (Arrow Natural Natural)
  | otherwise = error "Unknown primitive."


unify :: ConstraintSet -> Substitution
unify [] = []
unify ((t,u):cs)
  | t == u = unify cs
  -- Can we consatenate instead of append?
  | isTypeVar t && (not.isSubVar) t u = (unify cs) ++ [(,)]
  | otherwise = error "SUUUUUUPP!"


isTypeVar :: TypeSymbol -> Bool
isTypeVar (Variable _) = True
isTypeVar _ = False

isArrow :: TypeSymbol -> Bool
isArrow (Arrow _ _) = True
isArrow _ = False

isSubVar :: TypeSymbol -> TypeSymbol -> Bool
isSubVar t (List u) = t == u || isSubVar t u
isSubVar t (Arrow u s) = t == u || t == s || isSubVar t u || isSubVar t s
isSubVar _ _ = False

subType :: TypeSymbol -> TypeSymbol -> TypeSymbol -> TypeSymbol
subType t u r = t
subType t u r = u


-- Substitute every occurance of t with u
--subConstraints :: TypeSymbol -> TypeSymbol -> ConstraintSet -> ConstraintSet
--subConstraints t u ((s,r):cs) = (subType t u s, subType t u r):(subConstraints (t,u) cs)
