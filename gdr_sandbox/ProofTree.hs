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

-- Built-in typing relationships
builtins :: Ast.CstData -> Type
builtins InstCst val = Natural
builtins Constructor name arity = 
   case name of 
      "cons" -> Arrow $ Variable freshId $List Variable freshId
      "nil" -> List $ Variable freshId
      otherwise -> Error

builtins Primitive name arity = 
   case name of 
      "+" -> Arrow Natural Natural
      "-" -> Arrow Natural Natural
      "*" -> Arrow Natural Natural
      "/" -> Arrow Natural Natural
      otherwise -> Error

-- map the ast structure to formal rules
inftype :: Context -> Ast.Ast -> Type
inftype ctx (Constant constant) = builtins constant
inftype ctx (Variable name) = taut ctx name
inftype ctx (Applicaiton lhs rhs) = app ctx lhs rhs
inftype ctx (Lambda param body) =  abs ctx param body
inftype ctx (Let name input body) = let ctx name input body
