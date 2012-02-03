module Eval
( evalProgramInter
, getExprStr
) where

import Ast
import Environment



-- Given an environment, get the expression of a program
--eval :: Env -> Program -> Expression
--eval env (Prog exprs) = snd $ evalExprs env exprs

getExprStr :: Expression -> String
getExprStr (Num a) = show a
getExprStr _ = "Not implemented yet.    -Mgmt"

evalProgramInter :: Env -> Program -> (Env,Expression)
evalProgramInter env (Prog exprs) = evalExprs env exprs
evalProgramInter env ErrProg = (env, ErrExpr)

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
-- FIXME No need for new Env in call-by-value. Proof?
evalExpr env (Application (Id a) e) =
  let (newEnv,a') = evalExpr env (Id a) in evalExpr newEnv (Application a' e)
evalExpr env (Application _ _) = evalExpr env ErrExpr
-- Apply delta rules
evalExpr env (Binary op (Num a) (Num b)) =
  (env,deltaArith (Binary op (Num a) (Num b)))
-- Apply
evalExpr env (Id b) =
  case e of
    Nothing -> (env, ErrExpr)
    Just e' -> (env,e')
  where e = lookUp env b

evalExpr env (Num b) = (env, Num b)

-- As this is call by value, the term of a lambda cannot be reduced
evalExpr env (Lambda idExpr e) = (env,Lambda idExpr e)

evalExpr env (Binary op a b)
  | a' == a && b' == b = (env, (Binary op a b))
  | otherwise = evalExpr envModb (Binary op a' b')
  where (envModa,a') = evalExpr env a
        (envModb,b') = evalExpr envModa b

evalExpr env (Let (Id b) e) =
  let
    (envMode,e')= evalExpr env e
  in
    (bind envMode b e',e')
-- Any other let expressiion is wrong
evalExpr env (Let _ _) = (env, ErrExpr)
-- An error expression is an error
evalExpr env ErrExpr = (env, ErrExpr)
-- FIXME takes a special function, not yet supported
evalExpr env (Compound e) = (env,Compound e)



-- Delta rules for standard arithmetic operations covered by the SPU
deltaArith :: Expression -> Expression
deltaArith (Binary Add   (Num a) (Num b)) = Num (a + b)
deltaArith (Binary Minus (Num a) (Num b)) = Num (a - b)
deltaArith (Binary Mult  (Num a) (Num b)) = Num (a * b)
deltaArith (Binary Div   (Num a) (Num b)) = Num (a `quot` b)
deltaArith _ = error "Cannot be used on expressions that aren't arithmetic."

