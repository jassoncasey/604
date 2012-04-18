module Eval(
) where

data Constant a =
   Value a
   Function 
   deriving ( Show, Eq )

data Variable =
   Internal Int
   | External String
   deriving ( Show, Eq )

data Pattern =
   Var Variable
   | Tuple [Variable]
   | DataCons [Variable]
   deriving ( Show, Eq )

data Term =
   Con Constant
   | Var Variable
   | Abs Variable Term
   | App Term Term
   | Let Variable Term Term
   | Cnd Term Term Term
   | DataCons [Term]
   | Case Term [(Pattern, Term)]
   deriving ( Show, Eq )

-- return list of free variables
freeVars :: Term -> [Variable]
freeVars t = lfreeVars [] t
   where 
      lfreeVars :: [Variable] -> Term -> [Variable]
      lfreeVars bound (Con _) = []
      lfreeVars bound (Var x) = 
         if elem x bound
            then []
            else [x]
      lfreeVars bound (Abs x t) = 
         lfreeVars (x:bound) t
      lfreeVars bound (App t1 t2) = 
         (lfreeVars bound t1) ++ (lfreeVars bound t2)
      lfreeVars bound (Let x t1 t2) = 
         (lfreeVars bound t1) ++ (lfreeVars (x:bound) t2)
      lfreeVars bound (Cnd t1 t2 t3) =
         (lfreeVars bound t1) ++ (lfreeVars bound t2) ++ (lfreeVars bound t3)
      lfreeVars bound (Case t (cons, body)):tl = (freeVars bound t)
      lfreeVars bound (Case t []) = []
