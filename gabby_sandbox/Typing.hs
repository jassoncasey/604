module Ast where

import qualified Ast


data Type =
    Arrow Type Type
  | List Type
  | Natural
  | Unknown
  deriving (Show)

-- typing environment
-- FIXME: Move to own file?
type TypeContext = [(Ast.Name,Type)]

lookUp :: TypeContext -> String -> Maybe Type
lookUp (((Ast.Identifier nextSym),ty):cs) sym
  | sym == nextSym = Just ty
  | otherwise = lookUp cs sym
lookUp (((Ast.Unique _),ty):cs) sym = lookUp cs sym
lookUp _ _ = Nothing

bindVar :: TypeContext -> String -> Type -> TypeContext
bindVar context sym ty = ((Ast.Identifier sym),ty):context

constType :: Ast.Ast -> Type
constType (Ast.Constant (Ast.IntCst _)) = Natural
constType (Ast.Constant (Ast.Primitive sym _))
  | elem sym ["+","-","*","/"] = Arrow Natural (Arrow Natural Natural)
  | otherwise = error "Unknown primitive."
constType _ = error "Got a non-const type."


inferType :: TypeContext -> Ast.Ast -> Type
inferType _ _ = Natural


varType :: TypeContext -> Ast.Ast -> Type
varType context (Ast.Variable (Ast.Identifier sym)) =
  case ty of
    Just ty' -> ty'
    Nothing -> error ("Lookup failure. " ++ sym ++ " is a free variable.")
  where ty = lookUp context sym
varType _ _ = error "Not a variable."