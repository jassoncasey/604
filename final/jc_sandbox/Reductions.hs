module Reductions (
   Variable,
   Term,
   callByName,
   normalOrder,
   callByValue,
   applicativeOrder,
   hybridApplicativeOrder,
   headSpine,
   hybridNormalOrder
) where

import qualified Data.Set as Set
import qualified Data.Map as Map

-- constand structure
data Constant =
   Literal String                 -- literal
   | Delta String Int [Term]      -- delta function
   deriving ( Eq, Show, Ord )

-- variable structure
data Variable =
   Internal Int         -- internally generated
   | External String    -- user supplied
   deriving ( Eq, Show, Ord )

-- term structure
data Term =
   Error
   | Cons Constant
   | Var Variable
   | Abs Variable Term
   | App Term Term
   deriving ( Eq, Show )

allCons :: [Term] -> Bool
allCons terms = 
   where allCons' (term:terms) = 
            case term of 
               Cons _ -> allCons terms
               _ -> false
         allCons' [] = true

-- couple of type synonyms for delta function maps
type DeltaFunc = [Term] -> String
type DeltaMap  = Map String DeltaFunc

-- actual delta computations
delta_compute :: DeltaMap -> String -> [Term] -> Term
-- handle non-literals
delta_compute deltas name terms = 
   case fucntion of
      Just func -> if allCons terms
                     then Cons (Prim (function terms) 0 [])
                     else Cons (Prim name 0 terms)
      Nothing -> Cons (Prim name 0 terms)
   where function = lookup name deltas 
-- handle literals
delta_compute deltas name [] = Cons (Prim name 0 [])

-- delta function rules ... primary guards
delta :: ( Term -> Term ) -> DeltaMap -> Constant -> Term
delta eval ( Prim name airity terms )
   airity == 0 | = delta_compute deltas name (map eval terms)
   _ | = Prim name airity (map eval terms)
delta _ c = Cons c

-- return the set of free variables
freeVars :: Term -> Set Variable
freeVars t =
   lfreeVars empty t
   where lfreeVars :: Set Variable -> Term -> Set Variable
         -- no free variables in a constant
         lfreeVarsbound (Cons _) = empty
         -- handle a variable term
         lfreeVars bound (Var x) =
            if member x bound
               then empty
               else singleton x
         -- handle a abs term
         lfreeVars bound (Abs (Var x) t) =
            lfreeVars (insert x bound) t
         -- handle a app term
         lfreeVars bound (App t1 t2) =
            union (lfreeVars bound t1) (lfreeVars bound t2)

-- build a set of all variables from a term   
varInuse :: Term -> Set Nat
varInuse t =
   lvarInuse empty t 
   where -- local set colleciton of internal variable ids
         lvarAdd :: Variable -> Set Nat -> Set Nat
         lvarAdd (Internal id) set = insert id set
         lvarAdd _ set = set
         -- local variable in-use set collection
         lvarInuse :: Set Nat -> Term -> Set Nat 
         lvarInuse vars (Cons _) = empty
         lvarInuse vars (Var (Internal x)) = lvarAdd x vars 
         lvarInuse vars (Var _) = vars
         lvarInuse vars (Abs (Var x) P) = 
            union (lvarAdd x vars) (lvarInuse vars P)
         lvarInsue vars (App P Q) = 
            union (lvarInuse vars P) (lvarInuse vars Q)

-- find a fesh variable for this temr
freshVar :: Term -> Variable
freshVar t x =
   if null inuse
      then Variable (Internal 0)
      else Variable (Internal ((findMax inuse) + 1))
   where inuse = varInuse t

-- apply capture free substitution
-- 
-- [ N / x ] c = c(N) -- if airity(c) > 0
-- [ N / x ] x' = N
-- [ N / x ] a = a
-- [ N / x ] P Q = [ N / x ] P [ N / x ] Q
-- [ N / x ] \x.P = \x.P
-- [ N / x ] \y.P = \y.P --- if x not in FV(P)
-- [ N / x ] \y.P = \y. ( [ N / x ] P ) --- if x in FV(P), y not in FV(N)
-- [ N / x ] \y.P = \z. ( [ N / x][ z / y ] P ) --- if x in FV(P), y in FV(N)
--
apply :: Term -> Variable -> Term -> Term
-- handle a constant term
apply N x a@(Cons c) =
   case c of
      Prim name airity interms ->
         -- if this constant can still be applied
         if airity > 0 
            then Prim name (airity - 1) (N:interms)   -- return the new form
            else Cons c
      _ -> Cons c
-- handle a variable term
apply N x a@(Var x') = 
   if x == x' then N    -- [N/x] x' = N
      else a            -- [N/x] a = a
-- handle a abs term
apply N x t@(Abs x' P) =
   if x == x'
      then t               -- [N/x] \x.P = \x.P
      else apply' N x t    -- [N/x] \y.P = ...
   where apply' :: Term -> Variable -> Term -> Term
         apply' N x t@(Abs y P)

            -- if x not in FV(P)
            | not $ member x ( freeVars P ) = 
               t  -- [N/x] \y.P = \y.P

            -- if x in FV(P), y not in FV(N)
            | member x ( freeVars P )           
               && not $ member x ( freeVars N ) =
                  Abs y (apply tgt body param)   -- [N/x] \y.P = \y.([N/x] P)

            -- if x in FV(P), y in FV(N)
            | member x ( freeVars P )           
               && member y ( freeVars N ) =     -- [N/x] \y.P = \z.([N/x][z/y]P)
                  Abs z ( apply N x ( apply (Var z) y P ) )
                  where z = freshVar (App ( Abs x t ) N )
         -- dummy match to satisfy warnings
         apply' N x t = t
                  
-- handle an app term
apply N x (App P Q) =                           -- [N/x] P Q = [N/x] P [N/x] Q
   App ( apply N x P ) ( apply N x Q )

-- call by name reduction (cbn)
--
--                 x -> x
--
--     t_i----cbn---->t_i'   delta(t_i')---->v
--    -----------------------------------------
--                 c(t_i) ----> v
--
--           \x.e ----cbn----> \x.e
--
--  e1 ----cbn----> \x.e    e[e2/x] ----cbn----> e'
-- ---------------------------------------------------
--          ( e1 e2 ) ----cbn----> e'
--
--      e1 ----cbn----> e1' != \x.e    
-- --------------------------------------
--     ( e1 e2 ) ----cbn----> ( e1' e2 )
--
--
cbn :: Term -> Term
cbn Cons c = delta cbn c
cbn Var x = Var x
cbn Abs x e = Abs x e
cbn App e1 e2 = 
   case cbn e1 of
      Abs x e -> cbn ( apply e2 x e )
      e1' -> App e1' e2
callByName = cbn

-- normal order reduction (no)
--
--                                x -> x
--
--                  t_i----no---->t_i'   delta(t_i')---->v
--                -----------------------------------------
--                              c(t_i) ----> v
--
--                           e ----no----> e'
--                      -------------------------
--                        \x.e ----no----> \x.e'
--
--               e1 ----cbn----> \x.e    e[e2/x] ----no----> e'
--             -------------------------------------------------
--                       ( e1 e2 ) ----no----> e'
--
--   e1 ----cbn----> e1' != \x.e    e1' ----no----> e1'' e2 ----no----> e2'
--  ------------------------------------------------------------------------
--                   ( e1 e2 ) ----no----> ( e1'' e2' )
--
--
no :: Term -> Term
no Cons c = delta no c
no Var x = Var x
no Abs x e = Abs x (no e)
no App e1 e2 =
   case cbn e1 of
      Abs x e -> no (apply e2 x e)
      e1' -> App (no e1') (no e2)
normalOrder = no

-- call by value reduction (cbv)
--
--                                x -> x
--
--                t_i----cbv---->t_i'   delta(t_i')---->v
--                -----------------------------------------
--                               c(t_i) ----> v
--
--                        \x.e ----cbv----> \x.e
--
--    e1 ----cbv----> \x.e    e2 ----cbv----> e2'  e[e2'/x] ----cbv----> e'
--  -------------------------------------------------------------------------
--                      ( e1 e2 ) ----cbv----> e'
--
--           e1 ----cbv----> e1' != \x.e    e2 ----cbv----> e2'
--          ----------------------------------------------------
--                   ( e1 e2 ) ----cbv----> ( e1' e2' )
--
--
cbv :: Term -> Term
cbv Cons c = delta cbv c
cbv Var x = Var x
cbv Abs x e = Abs x e
cbv App e1 e2 = 
   case cbv e1 of
      Abs x e -> cbv ( apply ( cbv e2 ) x e ) 
      e1' -> App ( e1' ( cbv e2 ) )
callByValue = cbv

-- applicative order reduction (ao)
--
--                               x -> x
--
--                 t_i----ao---->t_i'   delta(t_i')---->v
--                -----------------------------------------
--                            c(t_i) ----> v
--
--                           e ----ao----> e'
--                        -----------------------
--                            \x.e ---> \x.e'
--
--    e1 ----ao----> \x.e    e2 ----ao----> e2'  e[e2'/x] ----ao----> e'
--  -------------------------------------------------------------------------
--                      ( e1 e2 ) ----ao----> e'
--
--             e1 ----ao----> e1' != \x.e    e2 ----ao----> e2'
--          ----------------------------------------------------
--                      ( e1 e2 ) ----ao----> ( e1' e2' )
--
--
ao :: Term -> Term
ao Cons c = delta ao c
ao Var x = Var x
ao Abs x e = Abs x (ao e)
ao App e1 e2 = 
   case ao e1 of
      Abs x e -> ao ( apply ( ao e2 ) x e ) 
      e1' -> App ( e1' ( ao e2 ) )
applicativeOrder = ao

-- hybrid applicative order reduction (hao)
--
--                               x -> x
--
--              t_i----hao---->t_i'   delta(t_i')---->v
--             -----------------------------------------
--                            c(t_i) ----> v
--
--                           e ----hao----> e'
--                        -----------------------
--                            \x.e ---> \x.e'
--
--    e1 ----cbv----> \x.e    e2 ----hao----> e2'  e[e2'/x] ----hao----> e'
--  -------------------------------------------------------------------------
--                      ( e1 e2 ) ----hao----> e'
--
--   e1 ----cbv----> e1' != \x.e    e1' ----hao----> e1'' e2 ----hao----> e2'
-- ----------------------------------------------------------------------------
--                      ( e1 e2 ) ----hao----> ( e1'' e2' )
--
--
hao :: Term -> Term
hao Cons c = delta hao c
hao Var x = Var x
hao Abs x e = Abx x (hao e)
hao App e1 e2 =
   case cbv e1 of
      Abs x e -> hao ( apply ( hao e2 ) x e )
      e1' -> App ( hao e1 ) ( hao e2 )
hybridApplicativeOrder = hao

-- head spine reduction (hs)
--
--                   x -> x
--
--     t_i----hs---->t_i'   delta(t_i')---->v
--    -----------------------------------------
--                 c(t_i) ----> v
--
--                e ----hs----> e'
--            -----------------------
--                \x.e ---> \x.e'
--
--   e1 ----hs----> \x.e    e[e2/x] ----hs----> e'
-- -------------------------------------------------
--             ( e1 e2 ) ----hno----> e'
--
--             e1 ----hs----> e1' != \x.e    
--       -------------------------------------
--          ( e1 e2 ) ----hs----> ( e1' e2)
--
--
hs :: Term -> Term
hs Cons c = delta hs c
hs Var x = Var x
hs Abs x e = Abs x ( hs e )
hs App e1 e2 =
   case hs e1 of
      Abs x e -> he ( apply e2 x e )
      e1' -> App e1' e2
headSpine = hs

-- hybrid normal order reduction (hno)
--
--                                     x -> x
--
--                    t_i----hno---->t_i'   delta(t_i')---->v
--                   -----------------------------------------
--                                  c(t_i) ----> v
--
--                                e ----hno----> e'
--                            -----------------------
--                                 \x.e ---> \x.e'
--
--                  e1 ----hs----> \x.e    e[e2/x] ----hno----> e'
--                -------------------------------------------------
--                            ( e1 e2 ) ----hno----> e'
--
--   e1 ----hs----> e1' != \x.e    e1' ----hno----> e1'' e2 ----hno----> e2'
-- ----------------------------------------------------------------------------
--                      ( e1 e2 ) ----hno----> ( e1'' e2' )
--
--
hno :: Term -> Term
hno Cons c = delta hno c
hno Var x = Var x
hno Abs x e = Abs x ( hno e )
hno App e1 e2 =
   case hs e1 of 
      Abs x e -> hno ( apply e2 x e )
      e1' -> App ( hno e1' ) ( hno e2 )
hybridNormalOrder = hno
