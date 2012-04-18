module Reductions (
) where

import qualified Data.Set as Set

-- variable structure
data Variable =
   Internal Int         -- internally generated
   | External String    -- user supplied
   deriving ( Eq, Show )

-- term structure
data Term =
   Var Variable
   | Abs Variable Term
   | App Term Term
   deriving ( Eq, Show )

-- return the set of free variables
freeVars :: Term -> Set Variable
freeVars t =
   lfreeVars empty t
   where lfreeVars :: Set Variable -> Term -> Set Variable
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
         lvarInuse vars (Var x) = lvarAdd x vars 
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
-- [ N / x ] x' = N
-- [ N / x ] a = a
-- [ N / x ] P Q = [ N / x ] P [ N / x ] Q
-- [ N / x ] \x.P = \x.P
-- [ N / x ] \y.P = \y.P --- if x not in FV(P)
-- [ N / x ] \y.P = \y. ( [ N / x ] P ) --- if x in FV(P), y not in FV(N)
-- [ N / x ] \y.P = \z. ( [ N / x][ z / y ] P ) --- if x in FV(P), y in FV(N)
--
apply :: Term -> Variable -> Term -> Term
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

-- call by name reduction
cbn :: Term -> Term
cbn Var x = Var x
cbn Abs x e = Abs x e
cbn App e1 e2 = 
   case cbn e1 of
      Abs x e -> cbn ( apply e2 x e )
      e1' -> App e1' e2

-- normal order reduction
no :: Term -> Term
no Var x = Var x
no Abs x e = Abs x (no e)
no App e1 e2 =
   case cbn e1 of
      Abs x e -> no (apply e2 x e)
      e1' -> App (no e1') (no e2)

-- call by value reduction
cbv :: Term -> Term
cbv Var x = Var x
cbv Abs x e = Abs x e
cbv App e1 e2 = 
   case cbv e1 of
      Abs x e -> cbv ( apply ( cbv e2 ) x e ) 
      e1' -> App ( e1' ( cbv e2 ) )

-- applicative order reduction
ao :: Term -> Term
ao Var x = Var x
ao Abs x e = Abs x (ao e)
ao App e1 e2 = 
   case ao e1 of
      Abs x e -> ao ( apply ( ao e2 ) x e ) 
      e1' -> App ( e1' ( ao e2 ) )

-- hybrid applicative order reduction
hao :: Term -> Term
hao Var x = Var x
hao Abs x e = Abx x (hao e)
hao App e1 e2 =
   case cbv e1 of
      Abs x e -> hao ( apply ( hao e2 ) x e )
      e1' -> App ( hao e1 ) ( hao e2 )

-- head spine reduction
hs :: Term -> Term
hs Var x = Var x
hs Abs x e = Abs x ( hs e )
hs App e1 e2 =
   case hs e1 of
      Abs x e -> he ( apply e2 x e )
      e1' -> App e1' e2

-- hybrid normal order reduction
hno :: Term -> Term
hno Var x = Var x
hno Abs x e = Abs x ( hno e )
hno App e1 e2 =
   case hs e1 of 
      Abs x e -> hno ( apply e2 x e )
      e1' -> App ( hno e1' ) ( hno e2 )
