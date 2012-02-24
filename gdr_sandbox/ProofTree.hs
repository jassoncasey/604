module ProofTree
where

import Ast

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

data Context = 
   Ctx {
      id    :: Int,
      reqs  :: Requirements,
      env   :: Environment
   }

data ProofTree = 
   Proof { 
      ctx   :: Context
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

-- generate a generic instance from a type scheme
instantiate :: Type -> Int -> [Int] -> ( Type, Int )
instantiate (Arrow lhs rhs) id bound = 
   ( Arrow lhs' rhs', idr' )
   where ( lhs', idl' ) = instantiate lhs id bound
         ( rhs', idr' ) = instantiate rhs id bound
instantiate (List type_) id bound =
   ( List r, id' )
   where ( r, id' ) = instantiate type_ id bound
instantiate (Forall arity type_) id bound = 
   instantiate type_ id (arity:bound)
instantiate Natural id bound = ( Natural, id )
instantiate (Alpha id) next_id bound = 
   if elem id bound 
      then ( Alpha next_id, next_id+1 )
      else ( Alpha id, next_id )

-- environment loookup
lookup :: Environment -> Ast.Name -> Maybe Type
lookup ((key, type_):tl) name =
   if key == name then Just type_
      else lookup tl name
lookup [] name = Nothing

-- type lookup wrapper
getName :: Environment -> Ast.Name -> Type
getName env name =
   case result of 
      Just r -> r
      Nohthing -> Error
   where result = lookup env name

-- Constant typing relationships
infConstType :: Context -> Ast.CstData -> Type
infConstType ctx (InstCst val) = Natural
infConstType ctx (Constructor name _) = getName (env ctx) name
infConstType ctx (Primitive name _) = getName (env ctx) name

-- Tautology rule
tautology :: Environment -> Ast.Name -> Type
tautology ctx name id = 
   case result of 
      Forall arity body -> instantiate 
      r -> r
   where result = getName (env ctx) name

-- abstraction rule
abstraction :: Context -> Ast.Name -> Ast.Ast -> Int -> ( Type, Int )
abstraction ctx param body id = 
   ( Arrow (Alpha id) type_, id' )
   where ( type_, id' ) = inftype ((param, Alpha id):ctx) body (id+1)

application :: Context -> Ast.Ast -> Ast.Ast -> Int -> ( Type, Int )
application ctx lhs rhs id = 
   

-- map the ast structure to formal rules
proofTree :: Context -> Ast.Ast -> ProofTree
proofTree ctx' t@(Constant constant) = 
   Proof{ ctx = ctx', term = t, type_ = type_', rule = "Const", prem = [] }
   where type_' = infConstType ctx' constant
proofTree ctx' t@(Variable name) = 
   Proof{ ctx = ctx', term = t, type_ = type_', rule = "Taut", prem = [] }
   where type_' = tautology ctx' name
proofTree ctx' t@(Applicaiton lhs rhs) = 
    Proof{ ctx = ctx', term = t, type_ = type_', rule = "App", prem = [] }
proofTree ctx' t@(Lambda param body) = 
    Proof{ ctx = ctx', term = t, type_ = type_', rule = "Abs", prem = [] }
proofTree ctx' t@(Let name input body) = 
    Proof{ ctx = ctx', term = t, type_ = type_', rule = "Let", prem = [] }
