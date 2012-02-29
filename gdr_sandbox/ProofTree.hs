module ProofTree
where

import Data.List( nub, intersperse, concat )
import Data.Maybe( fromJust )
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
         ctx'' = Ctx{nid=((nid ctx')+1), sub=(sub ctx'), env=(binding:(env ctx'))}

-- simple helper to build an arrow type
mkArrowType :: Type -> ProofTree -> Type
mkArrowType ltype typing = Arrow ltype (type_ typing)

-- simple function type extractor
getBodyType :: Type -> Type
getBodyType (Arrow _ rtype) = rtype
getBodyType x = x

getParamType :: Type -> Type
getParamType (Arrow ltype _) = ltype
getParamType _ = Error

-- unify these two types and return results with new substitutions
-- FIXME add error support
unify :: Substitution -> Type -> Type -> ( Type, Type, Substitution)
unify sub ltype rtype = (applySub sub' ltype, applySub sub' rtype, sub')
    where sub'' = unification [(ltype,rtype)]
          sub' = sub ++ sub''

-- Given a set of constraints, generate a substitution
unification :: [(Type,Type)] -> Substitution
unification [] = []
unification ((t,u):cs) =
  case (t,u) of
    _ | t == u -> unification cs
    ((Alpha n),_) | not $ elem n $ freeVars u ->
      (t,u):(unification $ map (\(a,b) -> (a,applySub [(t,u)] b)) cs)
    (_,(Alpha n)) | not $ elem n $ freeVars t ->
      (u,t):(unification $ map (\(a,b) -> (a,applySub [(u,t)] b)) cs)
    (Arrow t1 t2,Arrow u1 u2) -> unification ((t1,u1):(t2,u2):cs)
    (List t', List u') -> unification ((t',u'):cs)
    _ -> [(t,Error)]

applySub :: Substitution -> Type -> Type
applySub sub t
  | t == t' = t
  | otherwise = applySub sub t'
  where t' = applySub' sub t

-- Given a substitution, apply it to a type scheme
applySub' :: Substitution -> Type -> Type
applySub' [] type_ = type_
applySub' (s:ss) type_ =
    applySub' ss $ applySingleSub s type_
  where
    applySingleSub :: (Type,Type) -> Type -> Type
    applySingleSub (t,u) (List r) = List $ applySingleSub (t,u) r
    applySingleSub (t,u) (Arrow r s) =
        Arrow (applySingleSub (t,u) r) (applySingleSub (t,u) s)
    applySingleSub (t,u) r
        | t == r = u
        | otherwise = r

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
--instantiate Error n ns = error (show n ++ show ns )

-- Print the type. Has support for labeling alphas with 'tn' notation
getStrType :: Type -> String
getStrType t =
  let alphaStrMap = (zip (nub $ freeVars t) ['t':(show i) | i <- [(0::Int)..]])
  in printWithMap t alphaStrMap
    where
      printWithMap t m =
        case t of
          Natural   -> "Nat"
          List u    -> "[" ++ (printWithMap u m) ++ "]"
          Alpha n   -> fromJust $ lookup n m
          Arrow u r -> case u of
            Arrow _ _ -> "(" ++ (printWithMap u m) ++ ") -> "
              ++ (printWithMap r m)
            _ -> (printWithMap u m) ++ " -> " ++ (printWithMap r m)

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

-- application rule
-- FIXME Don't need to apply sub here. Can be a single application in typeTerm
proofTree ctx' t@(Application lhs rhs) =
   Proof{ctx=(Ctx{nid=nid', sub=sub',
         env=(env ctx')}), term=t, type_=type_'', rule="APP", 
         prem=[lproof,rproof]}
   where lproof = proofTree ctx' lhs
         rproof = proofTree (ctx lproof) rhs
         ( type_', nid' ) = genArrow (type_ lproof) (nid (ctx rproof))
         ptype = getParamType type_'
         (ltype, _, sub' ) = unify (sub (ctx rproof)) (ptype) (type_ rproof)
         type_'' = applySub sub' $ getBodyType type_'

-- let rule
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
  let
    (Proof ctx@(Ctx _ subs _) d type_' e f) = proofTree ctx0 term
    type_'' = applySub subs type_'
  in (Proof ctx d type_'' e f)



-- LaTeX Pretty printing
--FIXME filter out cons and nil
joinOn :: String -> [String] -> String
joinOn delim strs= concat $ intersperse delim strs

texBeginRule :: String -> String
texBeginRule s = "\\RightLabel{" ++ s ++ "}\n"

texPremises :: [ProofTree] -> String
texPremises prfs = 
  "\\AxiomC{" ++
  joinOn " \\hspace{1mm} " strs ++
  "}\n"
  where strs = map treeToTex prfs

texConclusion :: Int -> Context -> Type -> String -> String
texConclusion n c t conc =
  "\\" ++ premInf n ++ "{$" ++
  texContext c ++
  " \\vdash " ++
  conc ++
  " : " ++ texType t ++ "$}\n"
  where
    premInf :: Int -> String
    premInf 0 = "UnaryInfC"
    premInf 1 = "UnaryInfC"
    premInf 2 = "BinaryInfC"
    premInf _ = "TrinaryInfC"

texContext :: Context -> String
texContext (Ctx _ _ env) =  
  "\\{" ++ (joinOn ", " $ texBindings $ dropEnd 6 env) ++ "\\}"
  where
    texBindings :: Environment -> [String]
    texBindings [] = []
    texBindings ((Unique _,_):bs) = texBindings bs
    texBindings ((Identifier sym,t):bs) =
      (sym ++ " : " ++ (texType t)):texBindings bs

texType :: Type -> String
texType Natural = "\\mbox{Nat} "
texType (Alpha n) = "\\tau _{" ++ show n ++ "} "
texType Error = "\\perp "
texType (Forall n t) = "\\forall \\tau _{" ++ show n ++ "}." ++ texType t
texType (List t) = "\\left[" ++ texType t ++ "\\right]"
texType (Arrow t u) =
  case t of
    Arrow _ _ -> "(" ++ texType t ++ ")" ++ " \\rightarrow " ++ texType u
    _         -> texType t ++ " \\rightarrow " ++ texType u

texAst :: Ast -> String
texAst (Let name ast1 ast2) =
  "\\mbox{let} " ++ texAst ast1 ++ " in " ++ texAst ast2
texAst (Variable (Identifier sym)) = sym
texAst (Variable (Unique _)) = ""
texAst (Lambda sym ast) = "\\lambda " ++ sym ++ " . " ++ texAst ast
texAst (Application ast1 ast2) = texAst ast1 ++ " \\hspace{1mm}" ++ texAst ast2
texAst (Constant (IntCst n)) = show n
texAst (Constant (Constructor sym _)) = sym
texAst (Constant (Primitive sym _)) = sym

treeToTex' :: ProofTree -> String
treeToTex' ptree =
  "\\begin{prooftree}\n" ++
  treeToTex ptree ++
  "\\end{prooftree}\n"

treeToTex :: ProofTree -> String
treeToTex (Proof context constAst typ "TAUT" _) =
  "\\AxiomC{ }\n" ++ -- this is where you call the other stuff
  texBeginRule "T-TAUT" ++
  texConclusion 0 context typ infStr
  where
    infStr = texAst constAst


treeToTex (Proof context lambdaAst typ "ABS" premises) =
  joinOn " " (map treeToTex premises) ++
  texBeginRule "T-ABS" ++
  texConclusion (length premises) context typ infStr
  where
    infStr = texAst lambdaAst

treeToTex (Proof context appAst typ "APP" premises) =
  joinOn " " (map treeToTex premises) ++
  texBeginRule "T-APP" ++
  texConclusion (length premises) context typ infStr
  where
    infStr = texAst appAst

treeToTex (Proof context letAst typ "LET" premises) =
  joinOn " " (map treeToTex premises) ++
  texBeginRule "T-LET" ++
  texConclusion (length premises) context typ infStr
  where
    infStr = texAst letAst


treeToTex(Proof context _ typ shto premises) = error shto

texInDocTree :: ProofTree -> String
texInDocTree ptree =
  "\\documentclass[10pt]{article}\n" ++
  "\\usepackage{syntax}\n" ++
  "\\usepackage{bussproofs}\n" ++
  "\\usepackage{amsmath}\n" ++
  "\\usepackage{amssymb}\n" ++
  "\\usepackage{amsthm}\n" ++
  "\\begin{document}\n" ++
  "\\begin{center}\n" ++
  treeToTex' ptree ++
  "\\end{center}\n" ++
  "\\end{document}\n"


dropEnd :: Int -> [a] -> [a]
dropEnd 0 as = as
dropEnd _ [] = []
dropEnd n as = dropEnd (n - 1) $ init as














