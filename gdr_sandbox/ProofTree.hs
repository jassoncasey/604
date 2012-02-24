module ProofTree
where

import Ast
import Env

data Type = 
   Error
   | Arrow Type Type
   | List Type
   | Natural
   | Alpha Int
   | Forall Int Type
   deriving (Show,Eq)

type Requirements = [ ( Type, Type ) ]
type Environment  = [ ( Ast.Name, Type ) ]

data ProofTree = 
   Proof { 
      id    :: Int,
      reqs  :: Requirements,
      env   :: Environment,
      term  :: Ast.Ast,
      type_ :: Type,
      rule  :: String,
      prem  :: [ ProofTree ]
   }

-- built in type definitions
cons' = Forall 1 (Arrow (Alpha 1) (Arrow (List (Alpha 1)) (Alpha 1)))
nil'  = Forall 1 (Arrow (List (Alpha 1)) (Alpha 1))
binat = Arrow Natural (Arrow Natural Natural)

-- provide a helper for the base environment
base :: [ (Ast.Name, Type) ]
base = [ 
         (Ast.Identifier "cons", cons'), 
         (Ast.Identifier "nil" , nil'), 
         (Ast.Identifier "+", binat), 
         (Ast.Identifier "-", binat), 
         (Ast.Identifier "*", binat), 
         (Ast.Identifier "/", binat) 
       ]

-- environment loookup
lookup :: Environment -> Ast.Name -> Maybe Type
lookup ((key, type_):tl) name =
   if key == name then Just type_
      else lookup tl name
lookup [] name = Nothing

-- Constant typing relationships
typeConstant :: Environment -> Ast.CstData -> ProofTree
typeConstant env (InstCst val) = 
   Proof ( env, Natural 
typeConstant env (Constructor name _) = tautology env name
typeConstant env (Primitive name _) = tautology env name

-- Tautology rule
tautology :: Environment -> Ast.Name -> ProofTree
tautology ctx name id = 
   case type_ of
      Just type_' -> -- not sure how to get a generic instance here
      Nothing -> ( Error, id )
   where type_ = lookup ctx name

-- abstraction rule
abstraction :: Context -> Ast.Name -> Ast.Ast -> Int -> ( Type, Int )
abstraction ctx param body id = 
   ( Arrow (Alpha id) type_, id' )
   where ( type_, id' ) = inftype ((param, Alpha id):ctx) body (id+1)

application :: Context -> Ast.Ast -> Ast.Ast -> Int -> ( Type, Int )
application ctx lhs rhs id = 
   

-- map the ast structure to formal rules
proofTree :: Context -> Ast.Ast -> ProofTree
proofTree ctx (Constant constant) = 
   case 
   typeConstant ctx constant
proofTree ctx (Variable name) = tautology ctx name, id )
proofTree ctx (Applicaiton lhs rhs) = application ctx lhs rhs, id )
proofTree ctx (Lambda param body) = abstraction ctx param body, id )
proofTree ctx (Let name input body) = letb ctx name input body, id )
