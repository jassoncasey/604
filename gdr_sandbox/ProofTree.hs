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

type Substitution = [ ( Type, Type ) ]
type Environment  = [ ( Ast.Name, Type ) ]

data Context = 
   Ctx {
      id    :: Int,
      sub   :: Substitution,
      env   :: Environment
   }

data ProofTree = 
   Proof { 
      ctx   :: Context,
      term  :: Ast.Ast,
      type_ :: Type,
      rule  :: String,
      prem  :: [ ProofTree ]
   }

-- built in type definitions
cons' = Forall 1 (Arrow (Alpha 1) (Arrow (List (Alpha 1)) (List (Alpha 1))))
nil'  = Forall 2 (List (Alpha 2)) 
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

-- helpder to drop type variables bound more than once
type TypeBinding = ( Int, Int )
dropBinding :: Int -> [TypeBinding] -> [TypeBinding]
dropBinding id list =
   filter dropPredicate list
   where dropPredicate ( lhs, _ ) = id /= lhs

-- generate a generic instance from a type scheme
instantiate :: Type -> Int -> [(Int,Int)] -> ( Type, Int )
instantiate (Arrow lhs rhs) id bindings = 
   ( Arrow lhs' rhs', idr' )
   where ( lhs', idl' ) = instantiate lhs id bindings
         ( rhs', idr' ) = instantiate rhs idl' bindings
instantiate (List type_) id bindings =
   ( List r, id' )
   where ( r, id' ) = instantiate type_ id bindings
instantiate (Forall id type_) next_id bindings =
   instantiate type_ (next_id+1) ((id, next_id):bindings')
   where bindings' = dropBinding id bindings
instantiate Natural id bindings = ( Natural, id )
instantiate (Alpha id) next_id bindings = 
   ( Alpha (lookupBinding id bindings), next_id )
   where 
      lookupBinding id ((lhs, rhs):tail) = 
         if id == lhs 
            then rhs
            else lookupBinding id tail
      lookupBinding id [] = id

-- print the type
getStrType :: Type -> String
getStrType (Arrow lhs rhs) =
   (getStrType lhs) ++ "->" ++ (getStrType rhs)
getStrType (List type_) = 
   "[" ++ (getStrType type_) ++ "]"
getStrType (Forall id type_) = 
   "forall a_" ++ (show id) ++ "." ++ (getStrType type_)
getStrType Natural = 
   "Nat"
getStrType (Alpha id) = 
   "a_" ++ (show id)

-- ast.name printer
getStrName :: Ast.Name -> String
getStrName (Identifier name) = name
getStrName (Unique id) = "v_" ++ (show id)

-- environment printer
getStrEnvironment :: Environment -> String
getStrEnvironment env =
   "{ " ++ (makeString env) ++ "}"
   where 
      makeString :: Environment -> String
      makeString ((name, type_):tail) = 
            if null tail
               then (getStrName name) ++ ":" ++ (getStrType type_) ++ " " ++
                  (makeString tail)
               else (getStrName name) ++ ":" ++ (getStrType type_) ++ ", " ++
                  (makeString tail)                  
      makeString [] = ""
{-
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

-}
