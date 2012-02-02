

import Ast
import Environment

-- Test strings
-- Let Nat (Id Nat "t") (Binary Nat App (Lambda Nat (Id Nat "x") (Binary Nat Add (Id Nat "x") (Num Nat 3))) (Num Nat 3))
-- (Binary Nat App (Lambda Nat (Id Nat "x") (Lambda Nat (Id Nat "x") (Binary Nat Add (Id Nat "x")(Id Nat "x")))) (Num Nat 3))

-- Evaluate
--eval :: Env k a -> Expression -> Expression
--eval (Let t e1 e2) =

-- Reduce an expression into normal form
reduce :: Expression -> Expression
reduce (Binary t App a b)
  | reducible b = reduce b   -- Can reduce right
  | otherwise = case a of
    (Lambda _ _ _) -> betaReduce (Binary t App  a b)  -- Can reduce outer
    _              -> error "what" --(Binary t App  a b)
reduce (Let t e1 e2) = (Let t e1 (reduce e2))
reduce (Binary t op  (Num t1 a) (Num t2 b)) =
  deltaArith (Binary t op  (Num t1 a) (Num t2 b))
reduce e = e

-- Delta rules for standard arithmetic operations covered by the SPU
deltaArith :: Expression -> Expression
deltaArith (Binary Nat Add   (Num Nat a) (Num Nat b)) = Num Nat (a + b)
deltaArith (Binary Nat Minus (Num Nat a) (Num Nat b)) = Num Nat (a - b)
deltaArith (Binary Nat Mult  (Num Nat a) (Num Nat b)) = Num Nat (a * b)
deltaArith (Binary Nat Div   (Num Nat a) (Num Nat b)) = Num Nat (a `quot` b)
deltaArith _ = error "Cannot be used on expressions that aren't arithmetic."

-- Checks to see if expression is reducible, by value
reducible :: Expression -> Bool
reducible (Binary _ App e1 e2)
  | (not $ reducible e2) = case e1 of
    Lambda _ _ _ -> True
    _            -> False
  | otherwise = False
reducible (Let _ _ e2) = reducible e2
reducible (Binary _ Add e1 e2) = (reducible e1) && (reducible e2)
reducible (Binary _ Minus e1 e2) = (reducible e1) && (reducible e2)
reducible (Binary _ Mult e1 e2) = (reducible e1) && (reducible e2)
reducible (Binary _ Div e1 e2) = (reducible e1) && (reducible e2)
reducible _ = False

-- Beta reduction
betaReduce :: Expression -> Expression
betaReduce (Binary _ App (Lambda _ eid e) e') = exprIdReplace' e eid e'
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
exprIdReplace :: Expression -> Expression -> Expression -> Expression
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
exprIdReplace (Unary t op e) m r = Unary t op (exprIdReplace e m r)
exprIdReplace (Binary t op e1 e2) m r =
  Binary t op (exprIdReplace e1 m r) (exprIdReplace e2 m r)
exprIdReplace (Compound t exprs) m r =
  Compound t $ map (\e -> exprIdReplace e m r) exprs

-- accounts for lambda equivalence
exprIdReplace' :: Expression -> Expression -> Expression -> Expression
exprIdReplace' (Id t s) (Id t' s') rep =
  case idExpr of
    (Id t' s') -> rep
    _     -> idExpr
  where idExpr = (Id t s)
exprIdReplace' (Num t a) _ _ = Num t a
-- Warning! cannot overwrite other bound variables!
exprIdReplace' (Lambda t idExpr e) m r
  | idExpr == m = (Lambda t idExpr e)
  | otherwise = Lambda t idExpr (exprIdReplace e m r)
exprIdReplace' (Let t idExpr e) m r = Let t idExpr (exprIdReplace e m r)
exprIdReplace' (Unary t op e) m r = Unary t op (exprIdReplace e m r)
exprIdReplace' (Binary t op e1 e2) m r =
  Binary t op (exprIdReplace e1 m r) (exprIdReplace e2 m r)
exprIdReplace' (Compound t exprs) m r =
  Compound t $ map (\e -> exprIdReplace e m r) exprs

-- Returns a list of bound variables





-- Alpha equivalence


reduce' Binary t app (Lambda t e1 e2) = reduce' $ betaReduce (Lambda t e1 (reduce' e2))
-- Delta rule if it is not an application
reduce' 
reduce' (Binary t op (Num t1 a) (Num t2 b)) =
  deltaArith (Binary t op (Num t1 a) (Num t2 b))