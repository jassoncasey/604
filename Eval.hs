module Eval
( evalProgramInter
, getExprStr
) where

import Ast
import Environment
{-
-- Test strings
-- Let Nat (Id Nat "t") (
Binary Nat App (Lambda Nat (Id Nat "x") (Id Nat "x")) (Num Nat 3)-}
-- (Binary Nat App (Lambda Nat (Id Nat "x") (Lambda Nat (Id Nat "x") (Binary Nat Add (Id Nat "x")(Id Nat "x")))) (Num Nat 3))
-- Tests binary add
--let (e,t) = evalExpr Tip (Binary Add (Binary Add (Num 3) (Num 3)) (Binary Add (Num 3) (Num 3)))
--let (e1,t1) = evalExpr Tip (Let (Id "k") (Binary Add (Binary Add (Num 3) (Num 3)) (Binary Add (Num 3) (Num 3))))
--let (e1,t1) = evalAxpr Tip (


-- Evaluate
{-eval :: Env k a -> Expression -> Expression
eval env e
  case e of
    (Binary t App (Lambda t1 idExpr e) b) = let rb = eval env b in
      eval (insert (pushScope env) idExpr rb) e
    (Binary t op (Num t1 a) (Num t2 b)) = deltaArith (Binary t op  (Num t1 a) (Num t2 b))
    (Binary t op a b) =
    (Num a b) = e
--  | e /= e' = eval env $ e'
--  | otherwise = e
  where e' = reduce e-}



-- Given an environment, get the expression of a program
eval :: Env -> Program -> Expression
eval env (Prog exprs) = snd $ evalExprs env exprs

getExprStr :: Expression -> String
getExprStr (Num a) = show a
getExprStr _ = "Not implemented yet.    -Mgmt"

evalProgramInter :: Env -> Program -> (Env,Expression)
evalProgramInter env (Prog exprs) = evalExprs env exprs

evalExprs :: Env -> [Expression] -> (Env, Expression)
evalExprs env [] = (env, ErrExpr)
evalExprs env [e] = evalExpr env e
evalExprs env (e:es) =
  let (newEnv,_) = evalExpr env e in evalExprs newEnv es


-- Apply lambda
evalExpr :: Env-> Expression -> (Env,Expression)
-- Note, for this implementation, a let-bindings in b is in scope before lambda
evalExpr env (Application (Lambda (Id ident) e) b) =
  let
    (envModb,b') = evalExpr env b
    newEnv = (bind (pushScope envModb) ident b')
  in
    (envModb, snd $ evalExpr newEnv e) -- snd and using old scope works as pop
-- Apply delta rules
evalExpr env (Binary op (Num a) (Num b)) =
  (env,deltaArith (Binary op (Num a) (Num b)))
-- Apply
evalExpr env (Id b) =
  case e of
    Nothing -> (Tip, ErrExpr)
    Just e' -> (env,e')
  where e = lookUp env b

evalExpr env (Num b) = (env, Num b)

evalExpr env (Binary op a b)
  | a' == a && b' == b = (env, (Binary op a b))
  | otherwise = evalExpr env (Binary op a' b')
  where (envModa,a') = evalExpr env a
        (envModb,b') = evalExpr envModa b

evalExpr env (Let (Id b) e) =
  let
    (envMode,e')= evalExpr env e
  in
    (bind envMode b e',e')


-- Reduce an expression into normal form --assume only numbers
{-reduce :: Expression -> Expression
reduce (Binary t App (Lambda t1 idExpr e) b)
  | isRedex b = (Binary t App (Lambda t1 idExpr e) (reduce b))
  | otherwise = betaReduce (Binary t App (Lambda t1 idExpr e) b)

--reduce

reduce (Binary t op (Num t1 a) (Num t2 b)) =
  deltaArith (Binary t op  (Num t1 a) (Num t2 b))

reduce (Binary t op a b)
  | isRedex a = Binary t op (reduce a) b
  | isRedex b = Binary t op a (reduce b)
  | otherwise = (Binary t op a b)-}





reduce (Num b) = Num b
reduce (Id b) = Id b




isRedex :: Expression -> Bool
isRedex (Num _) = False
isRedex (Id _) = False --needs an environment lookup
isRedex _ = True

{-reduce (Binary t App a b)
  | reducible b = reduce b   -- Can reduce right
  | otherwise = case a of
    (Lambda _ _ _) -> betaReduce (Binary t App  a b)  -- Can reduce outer
    _              -> error "what" --(Binary t App  a b)
reduce (Let t e1 e2) = (Let t e1 (reduce e2))
reduce (Binary t op  (Num t1 a) (Num t2 b)) =
  deltaArith (Binary t op  (Num t1 a) (Num t2 b))
reduce e = e-}

-- Delta rules for standard arithmetic operations covered by the SPU
deltaArith :: Expression -> Expression
deltaArith (Binary Add   (Num a) (Num b)) = Num (a + b)
deltaArith (Binary Minus (Num a) (Num b)) = Num (a - b)
deltaArith (Binary Mult  (Num a) (Num b)) = Num (a * b)
deltaArith (Binary Div   (Num a) (Num b)) = Num (a `quot` b)
deltaArith _ = error "Cannot be used on expressions that aren't arithmetic."

-- Checks to see if expression is reducible, by value
reducible :: Expression -> Bool
{-reducible (Binary _ App e1 e2)
  | (not $ reducible e2) = case e1 of
    Lambda _ _ _ -> True
    _            -> False
  | otherwise = False-}
reducible (Let _ e2) = reducible e2
reducible (Binary Add e1 e2) = (reducible e1) && (reducible e2)
reducible (Binary Minus e1 e2) = (reducible e1) && (reducible e2)
reducible (Binary Mult e1 e2) = (reducible e1) && (reducible e2)
reducible (Binary Div e1 e2) = (reducible e1) && (reducible e2)
reducible _ = False

-- Beta reduction
betaReduce :: Expression -> Expression
betaReduce (Application (Lambda eid e) e') = exprIdReplace' e eid e'
betaReduce _ = error "Can only beta-reduce on an application to a lambda."

-- Alpha reduction
--alphaReduce :: Expression -> Expression -> Expression
--alphaReduce (Lambda t idExpr e) (Id t s) =
--  | 
--alphaReduce _ _ = error "Alpha reduction applies only to lambda expressions."


--cleanIdentifiers

-- Creates a list of bound variables from an expression
--boundVariables :: Expression -> [Expression]

--Replace all instances of an id in expression with the new expression
-- Take an expression, matches the second to the first
-- if match , replace with third expr
{-exprIdReplace :: Expression -> Expression -> Expression -> Expression
exprIdReplace (Id t s) (Id t' s') rep =
  case idExpr of
    (Id t' s') -> rep
    _     -> idExpr
  where idExpr = (Id t s)
exprIdReplace (Num t a) _ _ = Num t a
-- Warning! cannot overwrite other bound variables!
exprIdReplace (Lambda t idExpr e) m r =
  Lambda t idExpr (exprIdReplace e m r)
exprIdReplace (Let t idExpr e) m r = Let t idExpr (exprIdReplace e m r)
exprIdReplace (Binary t op e1 e2) m r =
  Binary t op (exprIdReplace e1 m r) (exprIdReplace e2 m r)
exprIdReplace (Compound t exprs) m r =
  Compound t $ map (\e -> exprIdReplace e m r) exprs-}

-- accounts for lambda equivalence
exprIdReplace' :: Expression -> Expression -> Expression -> Expression
exprIdReplace' (Id s) (Id s') rep =
  case idExpr of
    (Id s') -> rep
    _     -> idExpr
  where idExpr = (Id s)
exprIdReplace' (Num a) _ _ = Num a
-- Warning! cannot overwrite other bound variables!
exprIdReplace' (Lambda idExpr e) m r
  | idExpr == m = (Lambda idExpr e)
  | otherwise = Lambda idExpr (exprIdReplace' e m r)
exprIdReplace' (Let idExpr e) m r = Let idExpr (exprIdReplace' e m r)
exprIdReplace' (Binary op e1 e2) m r =
  Binary op (exprIdReplace' e1 m r) (exprIdReplace' e2 m r)
exprIdReplace' (Compound exprs) m r =
  Compound $ map (\e -> exprIdReplace' e m r) exprs
exprIdReplace' (Application e1 e2) m r =
  Application (exprIdReplace' e1 m r) (exprIdReplace' e2 m r)

-- Returns a list of bound variables





-- Alpha equivalence


{-reduce' Binary t app (Lambda t e1 e2) = reduce' $ betaReduce (Lambda t e1 (reduce' e2))
-- Delta rule if it is not an application
reduce' 
reduce' (Binary t op (Num t1 a) (Num t2 b)) =
  deltaArith (Binary t op (Num t1 a) (Num t2 b))-}



-- Beta reduction

