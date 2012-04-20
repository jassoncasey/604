module Steve.TypeCheck where

import Steve.Internal


-- Type binding environment type
type TypeBinding = (String, Type)

-- Initial type environment
initTypeBinding :: [TypeBinding]
initTypeBinding = [
  ("+", Func SNat $ Func SNat SNat),
  ("-", Func SNat $ Func SNat SNat),
  ("*", Func SNat $ Func SNat SNat),
  ("/", Func SNat $ Func SNat SNat),
  ("%", Func SBool $ Func SBool SBool),
  ("|", Func SBool $ Func SBool SBool) ]

typeOfConstant :: Constant -> Type
typeOfConstant (LitBool _) = SBool
typeOfConstant (LitNat  _) = SNat
typeOfConstant (LitChar _) = SChar

-- Get input and output types of a Func
typeOfFuncIn :: Type -> Maybe Type
typeOfFuncIn (Func typ _) = Just typ
typeOfFuncIn _ = Nothing
typeOfFuncOut :: Type -> Maybe Type
typeOfFuncOut (Func _ typ) = Just typ
typeOfFuncOut _ = Nothing


-- Type checker mark 1
-- This type checker only checks terms from the simply-typed lamdba calculus.
-- It is entirely based on the Maybe monad
-- Gee, wish I could make it smaller...
{-============================================================================-}

check :: [TypeBinding] -> Term -> Maybe Type

check env (Lit c) = Just $ typeOfConstant c

check env (Iden sym) = lookup sym env

check env (App e1 e2) = do
  typ1 <- check env e1
  typ2 <- check env e2
  argType <- typeOfFuncIn typ1
  ensure (argType == typ2)
  outType <- typeOfFuncOut typ1
  return outType

check env (Abs sym symType e) = check ((sym,symType):env) e

check env (If b e1 e2) = do
  condType <- check env b
  ensure (condType == SBool)
  typ1 <- check env e1
  typ2 <- check env e2
  ensure (typ1 == typ2)
  return typ1

{-============================================================================-}




-- Helper Functions
{-============================================================================-}
-- Think c-style assert =)
ensure :: Bool -> Maybe Type
ensure True = Just SNat
ensure False = Nothing