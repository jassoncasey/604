module Steve.TypeCheck where

import Steve.Internal

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


-- Type checkMer mark 1
-- This type checkMer only checkMs terms from the simply-typed lamdba calculus.
-- It is entirely based on the Maybe monad
-- Gee, wish I could make it smaller...
{-============================================================================-}

typeCheck :: [TypeBinding] -> Term -> Maybe Type
typeCheck ctors expr = checkM (ctors ++ initTypeBinding) expr

checkM :: [TypeBinding] -> Term -> Maybe Type

checkM env (Lit c) = Just $ typeOfConstant c

checkM env (Iden sym) = lookup sym env

checkM env (App e1 e2) = do
  typ1 <- checkM env e1
  typ2 <- checkM env e2
  argType <- typeOfFuncIn typ1
  ensureM (argType == typ2)
  outType <- typeOfFuncOut typ1
  return outType

checkM env (Abs sym symType e) = checkM ((sym,symType):env) e

checkM env (If b e1 e2) = do
  condType <- checkM env b
  ensureM (condType == SBool)
  typ1 <- checkM env e1
  typ2 <- checkM env e2
  ensureM (typ1 == typ2)
  return typ1

checkM env (Let sym typ func e) = do
  funcType <- checkM env func
  ensureM ( funcType == typ )
  checkM ((sym,typ):env) e

{-============================================================================-}




-- Helper Functions
{-============================================================================-}
-- Think c-style assert =)
ensureM :: Bool -> Maybe Type
ensureM True = Just SNat
ensureM False = Nothing

ensure :: Bool -> String -> Compute Type
ensure True _ = Good SNat
ensure False msg = Bad msg