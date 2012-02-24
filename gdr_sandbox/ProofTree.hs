module ProofTree
where

import Ast
import Env

data Type = 
   Error
   | Arrow Type Type
   | List Type
   | Natural
   | Variable Int
   | Forall Int Type
   deriving (Show,Eq)

-- typing context
type Context = [ ( Ast.Name, Type ) ]

-- context loookup
lookup :: Context -> Ast.Name -> Maybe Type
lookup ((key, type_):tl) name =
   if key == name then Just type_
      else lookup tl name
lookup [] name = Nothing

-- Constant typing relationships
typeConstant :: Ast.CstData -> Int -> ( Type, Int )
typeConstant (InstCst val) id = ( Natural, id )
typeConstant (Constructor name arity) id = 
   case name of 
      "cons" -> ( ( Arrow $ Variable id $List Variable id), id+1 )
      "nil" -> ( List $ Variable id, id+1 )
      otherwise -> ( Error, id )
typeConstant Primitive name arity = 
   case name of 
      "+" -> ( Arrow Natural ( Arrow Natural Natural ), id )
      "-" -> ( Arrow Natural ( Arrow Natural Natural ), id )
      "*" -> ( Arrow Natural ( Arrow Natural Natural ), id )
      "/" -> ( Arrow Natural ( Arrow Natural Natural ), id )
      otherwise -> ( Error, id )

-- Tautology rule
tautology :: Context -> Ast.Name -> Int -> ( Type, Int )
tautology ctx name id = 
   case type_ of
      Just type_' -> 
      Nothing -> ( Error, id )
   where type_ = lookup ctx name

-- map the ast structure to formal rules
inftype :: Context -> Ast.Ast -> Int -> ( Type, Int )
inftype ctx (Constant constant) id = ( typeConstant constant, id )
inftype ctx (Variable name) id = ( tautology ctx name, id )
inftype ctx (Applicaiton lhs rhs) id = ( app ctx lhs rhs, id )
inftype ctx (Lambda param body) id = ( abs ctx param body, id )
inftype ctx (Let name input body) id = ( let ctx name input body, id )
