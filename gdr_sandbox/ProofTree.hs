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
      nid   :: Int,
      sub   :: Substitution,
      env   :: Environment
   }
   deriving (Show,Eq)

data ProofTree = 
   Proof { 
      ctx   :: Context,
      term  :: Ast.Ast,
      type_ :: Type,
      rule  :: String,
      prem  :: [ ProofTree ]
   }
   deriving (Show,Eq)

-- generate an arrow type if necessary
genArrow :: Type -> Int -> ( Type, Int )
genArrow (List _) nid = ( Error, nid )
genArrow Natural nid = ( Error, nid )
genArrow (Forall _ _) nid = ( Error, nid )
genArrow (Alpha id') nid = ( (Arrow (Alpha nid) (Alpha (nid+1))), nid+2 )
genArrow t@(Arrow _ _) nid = ( t, nid )

-- build a universally quantified type
mkForallType :: [Int] -> Type -> Type
mkForallType (h:tail) type_ = 
   Forall h (mkForallType tail type_)
mkForallType [] type_ = type_

-- find all the free type variables in a type scheme
freeVars :: Type -> [Int]
freeVars type_ =
   free_vars type_ []
   where 
      free_vars :: Type -> [Int] -> [Int]
      free_vars Error _ = []
      free_vars Natural _ = []
      free_vars (Arrow lhs rhs) bound = 
         (free_vars lhs bound) ++ (free_vars rhs bound)
      free_vars (List type_) bound = free_vars type_ bound
      free_vars (Alpha nid) bound = 
         if elem nid bound
            then []
            else [nid]
      free_vars (Forall nid type_) bound = 
         free_vars type_ (nid:bound)

-- Collect all the free type variables in an environment
freeVarsEnv :: Environment -> [Int]
freeVarsEnv ((name, type_):tail) =
   (freeVars type_) ++ (freeVarsEnv tail)
freeVarsEnv [] = []

-- Find all the free type variables between two lists
availableVars :: [Int] -> [Int] -> [Int]
availableVars (h:tl) list =
   if elem h list 
      then availableVars tl list
      else (h:(availableVars tl list))
availableVars [] list = []

-- Build a let type
letType :: Type -> Environment -> Type
letType type_ env =
   mkForallType quant type_
   where quant = availableVars (freeVars type_) (freeVarsEnv env)

-- Add a binding to the context and update the counter
bindVar :: Context -> String -> (Context, Type)
bindVar ctx' name =
   ( ctx'', typevar )
   where typevar = Alpha (nid ctx')
         binding = ((Ast.Identifier name), typevar )
         ctx'' = Ctx{nid=(nid ctx'), sub=(sub ctx'), env=(binding:(env ctx'))}

-- simple helper to build an arrow type
mkArrowType :: Type -> ProofTree -> Type
mkArrowType ltype typing = Arrow ltype (type_ typing)

-- simple function type extractor
getBodyType :: Type -> Type
getBodyType (Arrow _ rtype) = rtype
getBodyType Error = Error
getBodyType x = x

-- unify these two types and return results with new substitutions
unify :: Substitution -> Type -> Type -> ( Type, Type, Substitution)
unify sub ltype rtype = ( ltype, rtype, sub )

-- produce the proper environment for the right premise of a let 
letBind :: Context -> Ast.Name -> Type -> Context
letBind ctx' name type_ = 
   Ctx{nid=(nid ctx'), sub=(sub ctx'), env=((name, type_'):(env ctx'))}
   where env' = env ctx'
         type_' = letType type_ env'

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

-- provide a helper for the initial context
ctx0 :: Context
ctx0 = Ctx{nid=3, sub=[], env=base}

-- helpder to drop type variables bound more than once
type TypeBinding = ( Int, Int )
dropBinding :: Int -> [TypeBinding] -> [TypeBinding]
dropBinding nid list =
   filter dropPredicate list
   where dropPredicate ( lhs, _ ) = nid /= lhs

-- generate a generic instance from a type scheme
instantiate :: Type -> Int -> [(Int,Int)] -> ( Type, Int )
instantiate (Arrow lhs rhs) nid bindings = 
   ( Arrow lhs' rhs', idr' )
   where ( lhs', idl' ) = instantiate lhs nid bindings
         ( rhs', idr' ) = instantiate rhs idl' bindings
instantiate (List type_) nid bindings =
   ( List r, nid' )
   where ( r, nid' ) = instantiate type_ nid bindings
instantiate (Forall nid type_) next_id bindings =
   instantiate type_ (next_id+1) ((nid, next_id):bindings')
   where bindings' = dropBinding nid bindings
instantiate Natural nid bindings = ( Natural, nid )
instantiate (Alpha nid) next_id bindings = 
   ( Alpha (lookupBinding nid bindings), next_id )
   where 
      lookupBinding nid ((lhs, rhs):tail) = 
         if nid == lhs 
            then rhs
            else lookupBinding nid tail
      lookupBinding nid [] = nid

-- print the type
getStrType :: Type -> String
getStrType (Arrow lhs rhs) =
   (getStrType lhs) ++ "->" ++ (getStrType rhs)
getStrType (List type_) = 
   "[" ++ (getStrType type_) ++ "]"
getStrType (Forall nid type_) = 
   "forall a_" ++ (show nid) ++ "." ++ (getStrType type_)
getStrType Natural = 
   "Nat"
getStrType (Alpha nid) = 
   "a_" ++ (show nid)

-- ast.name printer
getStrName :: Ast.Name -> String
getStrName (Identifier name) = name
getStrName (Unique nid) = "v_" ++ (show nid)

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

-- environment loookup
lookupType :: Environment -> Ast.Name -> Type
lookupType ((key, type_):tl) name =
   if key == name then type_
      else lookupType tl name
lookupType [] name = Error

-- Constant and variable typing relationships
infConstType :: Context -> Ast.CstData -> ( Context, Type )
infConstType ctx (IntCst val) = ( ctx, Natural )
infConstType ctx ( Constructor name _ ) = 
   (Ctx { nid=nid', env=(env ctx), sub=(sub ctx) }, type_ )
   where generic = lookupType (env ctx) (Ast.Identifier name)
         ( type_, nid' ) = instantiate generic (nid ctx) []
infConstType ctx ( Primitive name _ ) = 
   (Ctx { nid=nid', env=(env ctx), sub=(sub ctx) }, type_ )
   where generic = lookupType (env ctx) (Ast.Identifier name)
         ( type_, nid' ) = instantiate generic (nid ctx) []
infNameType :: Context -> Ast.Name -> ( Context, Type )
infNameType ctx name =
   (Ctx { nid=nid', env=(env ctx), sub=(sub ctx) }, type_ )
   where generic = lookupType (env ctx) name
         ( type_, nid' ) = instantiate generic (nid ctx) []

-- Main typing function
proofTree :: Context -> Ast.Ast -> ProofTree

-- Tautology rule
proofTree ctx' t@(Constant val) = 
   Proof{ctx=ctx'', term=t, type_=type_', rule="TAUT", prem=[]}
   where ( ctx'', type_' ) = infConstType ctx' val
proofTree ctx' t@(Variable val) = 
   Proof{ctx=ctx'', term=t, type_=type_', rule="TAUT", prem=[]}
   where ( ctx'', type_' ) = infNameType ctx' val

-- abstraction rule
proofTree ctx' t@(Lambda param body) = 
   Proof{ctx=(Ctx{nid=(nid (ctx premise)), sub=(sub (ctx premise)), 
         env=(env ctx')}), term=t, type_=type_'', rule="ABS", prem=[premise]}
   where ( ctx'', type_' ) = bindVar ctx' param
         premise = proofTree ctx'' body
         type_'' = mkArrowType type_' premise

-- application rule ... not done
proofTree ctx' t@(Application lhs rhs) =
   Proof{ctx=(Ctx{nid=nid', sub=sub',
         env=(env ctx')}), term=t, type_=type_'', rule="APP", 
         prem=[lproof,rproof]}
   where lproof = proofTree ctx' lhs
         rproof = proofTree (ctx lproof) rhs
         ( type_', nid' ) = genArrow (type_ lproof) (nid (ctx rproof))
         (ltype, rtype, sub' ) = unify (sub (ctx rproof)) type_' (type_ rproof)
         type_'' = getBodyType ltype

-- let rule .... not done
proofTree ctx' t@(Let name lhs rhs) =
   Proof{ctx=(Ctx{nid=(nid (ctx rproof)), sub=(sub (ctx rproof)),
         env=(env ctx')}), term=t, type_=type_', rule="LET", 
         prem=[lproof,rproof]}
   where lproof = proofTree ctx' lhs
         rproof = proofTree (letBind (ctx lproof) name (type_ lproof)) rhs
         type_' = type_ rproof

-- main entry point to build a proof tree
typeTerm :: Ast.Ast -> ProofTree
typeTerm term =
   proofTree ctx0 term
