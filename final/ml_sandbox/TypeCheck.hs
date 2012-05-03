module Steve.TypeCheck where

import Steve.Internal


{-

Initial typebindings are not complete

-}


-- TypeCheck Monad
{-============================================================================-}
-- I know either can be used, but I had to learn how to do it myself
data TypeChecker a = Good a | Bad String deriving (Show,Eq)

instance Monad TypeChecker where
  return x = Good x
  Good x >>= f = f x
  Bad msg >>= f = Bad msg
  fail msg = Bad msg

-- Take a list of type checked things and return a typechecked list.
-- If any element is Bad, return bad. Otherwise, return a list of Good
fromTypeCheckList :: [TypeChecker a] -> TypeChecker [a]
fromTypeCheckList [] = Good []
fromTypeCheckList ((Good x):xs) = fromTypeCheckList xs >>= \xs' -> return (x:xs')
fromTypeCheckList ((Bad msg):_) = Bad msg

mapTC :: (a -> TypeChecker b) -> [a] -> TypeChecker [b]
mapTC f xs = fromTypeCheckList $ map f xs
{-============================================================================-}


-- Initial type environment
initTypeBinding :: [TypeBinding]
initTypeBinding = [
  ("+", Func SNat $ Func SNat SNat),
  ("-", Func SNat $ Func SNat SNat),
  ("*", Func SNat $ Func SNat SNat),
  ("/", Func SNat $ Func SNat SNat),
  ("%", Func SNat $ Func SNat SNat),
  ("not", Func SBool SBool),
  ("or", Func SBool $ Func SBool SBool),
  ("and", Func SBool $ Func SBool SBool),
  ("&", Func SNat $ Func SNat SNat),
  ("&", Func SNat $ Func SNat SNat),
  ("&", Func SNat $ Func SNat SNat),
  ("&", Func SNat $ Func SNat SNat) ]


-- Type checkM1er mark 1
-- This type checkM1er only checkM1s terms from the simply-typed lamdba calculus.
-- It is entirely based on the Maybe monad
-- Gee, wish I could make it smaller...
{-============================================================================-}

typeCheckM1 :: [TypeBinding] -> Term -> Maybe Type
typeCheckM1 ctors expr = checkM1 (ctors ++ initTypeBinding) expr

checkM1 :: [TypeBinding] -> Term -> Maybe Type

checkM1 env (Lit c) = Just $ typeOfConstant c

checkM1 env (Iden sym) = lookup sym env

checkM1 env (App e1 e2) = do
  typ1 <- checkM1 env e1
  typ2 <- checkM1 env e2
  argType <- typeOfFuncInM typ1
  ensureM (argType == typ2)
  outType <- typeOfFuncOutM typ1
  return outType

checkM1 env (Abs sym symType e) = checkM1 ((sym,symType):env) e

checkM1 env (If b e1 e2) = do
  condType <- checkM1 env b
  ensureM (condType == SBool)
  typ1 <- checkM1 env e1
  typ2 <- checkM1 env e2
  ensureM (typ1 == typ2)
  return typ1

checkM1 env (Let sym typ func e) = do
  funcType <- checkM1 env func
  ensureM ( funcType == typ )
  checkM1 ((sym,typ):env) e

{-============================================================================-}



-- Typechecker mark 2
-- Has limited diagnostic support
{-============================================================================-}
typeCheckM2 :: [TypeBinding] -> Term -> TypeChecker Type
typeCheckM2 ctors expr = check (ctors ++ initTypeBinding) expr

check :: [TypeBinding] -> Term -> TypeChecker Type

--   c:T ∊ Γ
-- -------------- T-CONST
--   Γ ⊢ c:T
check env (Lit c) = return $ typeOfConstant c

--   x:T ∊ Γ
-- ----------------- T-VAR
--   Γ ⊢ x:T
check env (Iden x) =
  case lookup x env of
    Just t  -> return t
    Nothing -> fail ("Not in scope: '" ++ x ++ "'")

--          T:T ∊ Γ
-- -------------------------- T-PDU
--   Γ ⊢ T {x1,x2,...,xn}:T
-- FIXME This should introduce type information to the fact environment
check env (Pdu t) = return t

--   Γ,(x:T) ⊢ e:U
-- ----------------- T-ABS
--  Γ ⊢ λx:T.e:T→U
check env (Abs x t e) = do
  u <- check ((x,t):env) e
  return $ Func t u

--   Γ ⊢ e1:U → T   Γ ⊢ e2:U
-- --------------------------- T-APP
--        Γ ⊢ e1 e2:T
check env (App e1 e2) = do
  u_t <- check env e1
  u <- check env e2
  u' <- typeOfArg u_t
  checkSameType u' u "Error with T-APP:"
  t <- typeOfOut u_t
  return t

--   Γ ⊢ e1:Bool   Γ ⊢ e2:T   Γ ⊢ e3:T
-- ------------------------------------- T-IF
--      Γ ⊢ if e1 then e2 else e3:T
check env (If e1 e2 e3) = do
  conditionType <- check env e1
  checkSameType SBool conditionType "Error with T-IF conditional:"
  t  <- check env e2
  t' <- check env e3
  checkSameType t t' "Error with T-IF branches:"
  return t

--   Γ ⊢ e1:T    Γ,(x:T) ⊢ e2:U
-- ------------------------------ T-Let
--    Γ ⊢ let x = e1:T in e2:U
check env (Let x t e1 e2) = do
  t' <- check env e1
  checkSameType t t' "Error with T-LET:"
  check ((x,t):env) e2

--   Γ ⊢ e : T
--   for each case i
--     Ki:T1 → T2 → ... → Tn → T ∊ Γ
--     Γ,(a1:T1),(a2:T2),...,(an:Tn) ⊢ ei:U
-- --------------------------------------------------- T-CASE
--   Γ ⊢ case e of {K1 a1 a2 ... -> e1; K2 b1 b2 ... -> e2; ...}:U
check env (Case e cases) = do
  t <- check env e
  -- lookup each constructor...
  -- verify params of each constructor
  -- check each case
  us <- mapTC checkCase cases
  checkSameList (head us) us "Error with T-CASE clause types:"
  return $ head us
  where
    checkCase :: (String,[TypeBinding],Term) -> TypeChecker Type
    checkCase (_,newBindings,e) = check (newBindings ++ env) e


{-============================================================================-}




-- Helper Functions
{-============================================================================-}
-- Think c-style assert =)
ensureM :: Bool -> Maybe Type
ensureM True = Just SNat
ensureM False = Nothing

ensure :: Bool -> String -> TypeChecker Type
ensure True _ = Good SNat
ensure False msg = Bad msg


typeOfConstant :: Constant -> Type
typeOfConstant (LitBool _) = SBool
typeOfConstant (LitNat  _) = SNat
typeOfConstant (LitChar _) = SChar

-- Get input and output types of a Func
typeOfFuncInM :: Type -> Maybe Type
typeOfFuncInM (Func typ _) = Just typ
typeOfFuncInM _ = Nothing
typeOfFuncOutM :: Type -> Maybe Type
typeOfFuncOutM (Func _ typ) = Just typ
typeOfFuncOutM _ = Nothing

typeOfArg :: Type -> TypeChecker Type
typeOfArg (Func typ _) = Good typ
typeOfArg _ = Bad "Not a function type."
typeOfOut :: Type -> TypeChecker Type
typeOfOut (Func _ typ) = Good typ
typeOfOut _ = Bad "Not a function type."

-- errors
checkSameType :: Type -> Type -> String -> TypeChecker Type
checkSameType t u str = if t == u
  then return SNat
  else Bad (str ++ " Couldn't match expected type '"++ show t
           ++ "' with type '" ++ show u ++ "'.")

checkSameList :: Type -> [Type] -> String -> TypeChecker Type
checkSameList t ts str = if all (== t) ts
  then return SNat
  else Bad (str ++ "Couldn't match type " ++ show t ++ " with the types "
    ++ show ts ++ ".")