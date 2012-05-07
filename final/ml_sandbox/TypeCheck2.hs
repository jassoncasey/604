module Steve.TypeCheck2 where

-- This file defines the typechecker/kinder for Steve



import Steve.Internal







-- Initialize Gamma
gammaInit :: [Gamma]
gammaInit =
  [ KindBind SNat' Star,
    KindBind SBool' Star,
    KindBind (Uint' (Lit' (LitNat 0)) (Lit' (LitNat 0))) Star,
    KindBind (Uint' (Lit' (LitNat 16)) (Lit' (LitNat 0))) Star,
    KindBind (Uint' (Lit' (LitNat 32)) (Lit' (LitNat 0))) Star,
    TypeBind "neg" (Func' SNat' SNat'),
    TypeBind "+" (Func' SNat' $ Func' SNat' SNat'),
    TypeBind "-" (Func' SNat' $ Func' SNat' SNat'),
    TypeBind "*" (Func' SNat' $ Func' SNat' SNat'),
    TypeBind "/" (Func' SNat' $ Func' SNat' SNat'),
    TypeBind "%" (Func' SNat' $ Func' SNat' SNat'),
    TypeBind "<" (Func' SNat' $ Func' SNat' SBool'),
    TypeBind ">" (Func' SNat' $ Func' SNat' SBool'),
    TypeBind "<=" (Func' SNat' $ Func' SNat' SBool'),
    TypeBind ">=" (Func' SNat' $ Func' SNat' SBool'),
    TypeBind "<>" (Func' SNat' $ Func' SNat' SBool'),
    TypeBind "==" (Func' SNat' $ Func' SNat' SBool')
  ]



-- KindingRules
kind :: Type' -> [Gamma] -> TypeChecker Kind
kind t g = do
  --k <- kindLookup t g
  return Star --k





-- The top leveltype checker
{-============================================================================-}

topLevelChecker :: ([Sigma],[Gamma]) -> [Ast] -> TypeChecker Type'

-- Empty program
topLevelChecker _ [] = Good SNat'

--    Σ|Γ ⊢ T::*    Σ|Γ ⊢ t:τ    Σ|Γ,<f:τ> ⊢ prgm
--  ------------------------------------------------ TopLevelLet
--            Σ|Γ ⊢ let f = t:τ, prgm
topLevelChecker (sigma,gamma) ((Let' n t e1 e2):asts) = do
  kt  <- kind t gamma
  typ <- check (sigma,gamma) e1
  topLevelChecker (sigma,(TypeBind n t):gamma) asts

-- Σ|Γ ⊢ pdu id {l1 = τ1,...,ln = τn}:ρ    Σ:<id:ρ>|Γ,(id::*) ⊢ prgm
-- ----------------------------------------------------------------- TopLevelPdu
--           Σ|Γ ⊢ pdu id {l1 = τ1,...,ln = τn}, prgm
topLevelChecker (sigma,gamma) ((PduDef n feilds):asts) = do
  notInSigma n sigma
  p <- check (sigma,gamma) (PduDef n feilds)
  topLevelChecker ((n,p):sigma,(KindBind p Star):gamma) asts


topLevelChecker _ _ = Bad "A non-pdu, non function was caught at top level."

{-============================================================================-}





{-============================================================================-}


-- type check
check :: ([Sigma],[Gamma]) -> Ast -> TypeChecker Type'


--    Σ|Γ ⊢ <<>,l1:τ1>:ρ1    ∀i∊1..n Σ|Γ ⊢ <ρ_i-1,l_i:τ_i>:ρi
--  -------------------------------------------------------------LLRecord
--           Σ|Γ ⊢ pdu id {l1 = τ1,...,ln = τn}:ρn
check (sigma,gamma) (PduDef n (f:fs)) = do
  p1' <- check (sigma,gamma) $ PduDefPart [f]
  let (Rec' p1) = p1'
  pn <- unRoll p1 fs -- This handles the forall checks
  return $ Rec' pn
    where
      unRoll :: [(String,Type')] -> [(String,Type')]
                -> TypeChecker [(String,Type')]
      unRoll [] pn' = Good pn'
      unRoll (f':fs') pi_1 = do
          	p_i' <- check (sigma,gamma) (PduDefPart (f':pi_1))
          	let (Rec' p_i) = p_i'
          	p_n <- unRoll fs' p_i
          	return p_n


--
--  ----------------------------------------- LLRecordPartial
--
check (sigma,gamma) (PduDefPart rec@((l,t):rho)) = do -- ambiguous ...
  notIn l rho
  let labels = lunzip rec
  let lilGamma = foldl b (Rec' rec) labels --betaReduceType l ("$rho." ++ l) rho
  let t' = foldl b t labels
  kt <- kind t gamma
  return $ Rec' ((l,t'):rho)
    where b :: Type' -> String -> Type'
    	  b fs l = betaReduceType l ("$rho." ++ l) fs
  

--   c:T ∊ Γ
-- -------------- T-CONST
--   Γ ⊢ c:T
check (sigma,gamma) (Lit' c) = return $ typeOfConstant c

--   x:T ∊ Γ
-- ----------------- T-VAR
--   Γ ⊢ x:T
check (sigma,gamma) (Iden' x) = typeLookup x gamma

--   Γ,(x:T) ⊢ e:U
-- ----------------- T-ABS
--  Γ ⊢ λx:T.e:T→U
check (sigma,gamma) (Abs' x t e) = do
  k <- kind t gamma
  u <- check (sigma,(TypeBind x t):gamma) e
  return $ Func' t u

--   Γ ⊢ e1:U → T   Γ ⊢ e2:U
-- --------------------------- T-APP
--        Γ ⊢ e1 e2:T
check (sigma,gamma) (App' e1 e2) = do
  u_t <- check (sigma,gamma) e1
  u   <- check (sigma,gamma) e2
  u'  <- typeOfArg u_t
  checkSameType u' u "Error with T-APP:"
  t   <- typeOfOut u_t
  return t

--   Γ ⊢ e1:Bool   Γ ⊢ e2:T   Γ ⊢ e3:T
-- ------------------------------------- T-IF
--      Γ ⊢ if e1 then e2 else e3:T
check (sigma,gamma) (If' e1 e2 e3) = do
  conditionType <- check (sigma,gamma) e1
  checkSameType SBool' conditionType "Error with T-IF conditional:"
  t  <- check (sigma,gamma) e2
  t' <- check (sigma,gamma) e3
  checkSameType t t' "Error with T-IF branches:"
  return t

--   Σ|Γ ⊢ t:ρ    <ρ.l:τ> ∊ ρ
-- -------------------------------------------- Projection
--   Σ|Γ ⊢ t.l:[t/ρ]τ
check (sigma,gamma) (Proj' e l) = do
  p <- check (sigma,gamma) e
  let (UserType' pName) = p
  pRec <- sigmaLookup pName sigma  -- This gets the actual record
  t <- rhoLookup ("$rho."++l) pRec
  return $ rhoReplaceType e t

check _ t = Bad ("Not implemented:" ++ show t)



--betaReduceTerm
--betaReduceTerm
--betaReduceTerm
{-
    Iden' String
  | Lit' Constant
  | Pdu' Type'     -- Rename this to PduCtor
  | PduDef String [(String,Type')]
  | Abs' String Type' Ast
  | App' Ast Ast
  | If' Ast Ast Ast
  | Let' String Type' Ast Ast
  | Case' Ast [(String,[TypeBinding],Ast)]
  | Proj' Ast String 
  | Nil-}


{-
data Type' =
    SNat'
  | SChar'
  | SBool'
  | List' Type'
  | Func' Type' Type'
  | TVar' String
  | UserType' String
  | Array' Type' Ast
  | Uint' Ast Ast
  | Dep' Ast Type' Type'
  | Rec' [(String,Type)]
  | Rec1' RecordType
  deriving (Show,Eq)
  -}