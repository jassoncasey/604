module Steve.Ast where


import Steve.Internal


{-
  The AST file contains all code to convert a parse tree into an AST and to
  convert type declarations into type tables and constructors.

  FIXMES
    - rewrite monadically. We need error support!
    - toType needs to see the typeBinding environment. right now, it's passed []
    - make sure all ctors are accounted for in case expressions
-}

{-
interface:
  getAst
  getTypeTables

-}

{-
The parse tree to AST transformation is complicated so here is a sketch:
  Assuming a successful transformation, we get a term, a list of types, a list
    of records and a type binding environment
  1. split declarations into type declarations and function declarations
    a. process type declarations, get back list of user defined type, a list of
       pdu records and typeBinding environment created from the adt value
       constructors and pdu fields
      i. 
  2.
-}

-- Process declarations
-- The result of parseUnit is just a declaration list. processDecls converts the
-- list of declarations into a list of top-level functions and a type-table.
{-============================================================================-}

-- takes declarations, returns an AST, list of ADT names, list of PDU records
-- and a list of constructors with which to initialize the environment
processDecls :: [Declaration] ->
                Maybe (Term, [String],[(String,[(String,Type)])], [TypeBinding])
processDecls decls = do
  let (adtNames,ctors) = getAdtNamesAndCtors $ filterAdtDecls decls
  let (pduRecs,accessors) = getPduRecordsAndAccessors $ filterPduDecls decls
  let typeBindings = ctors ++ accessors
  let funcs = getFuncsFromDecls decls
  prgm <- functionsToAst funcs typeBindings
  return (prgm, adtNames, pduRecs, typeBindings)

{-============================================================================-}


-- Function conversion helper functions
-- These functions are reponsible for parts of the top-level function conversion
{-============================================================================-}
getFuncsFromDecls :: [Declaration] -> [TopLevelFunc]
getFuncsFromDecls [] = []
getFuncsFromDecls [FuncDecl f] = [f]
getFuncsFromDecls [_] = []
getFuncsFromDecls ((FuncDecl f):decls) = f:(getFuncsFromDecls decls)
getFuncsFromDecls ((_):decls) = getFuncsFromDecls decls


-- Given a list of types, a list of params and a term (function body), produce
-- an abstraction over the term.
makeAbs :: [Type] -> [String] -> Term -> Maybe Term
makeAbs [t] [] e = Just e
makeAbs []  _  _ = Nothing
makeAbs  _  [] _ = Nothing
makeAbs (typ:typs) (param:params) e =
  case makeAbs typs params e of
    Just e' -> Just $ Abs param typ e'
    Nothing -> Nothing

-- Takes a type and attempts to split it up into an n parameter function
-- Result is a list of n+1 types or nothing. The extra type is the result type
tryBreakType :: Int -> Type -> Maybe [Type]
tryBreakType 0 t = Just [t]
tryBreakType n (Func t1 t2) =
  case tryBreakType (n-1) t2 of
    Just t  -> Just (t1 : t)
    Nothing -> Nothing
tryBreakType _ t = Nothing

parseTreeToAst :: PTree -> [TypeBinding] -> Maybe Term
parseTreeToAst (Identifier name) _ = Just $ Iden name
parseTreeToAst (Literal c) _ = Just $ Lit c
parseTreeToAst (Application t1 t2) typBindings = do
  t1' <- parseTreeToAst t1 typBindings
  t2' <- parseTreeToAst t2 typBindings
  return $ App t1' t2'
parseTreeToAst (IfThenElse t1 t2 t3) typBindings = do
  t1' <- parseTreeToAst t1 typBindings
  t2' <- parseTreeToAst t2 typBindings
  t3' <- parseTreeToAst t3 typBindings
  return $ If t1' t2' t3'
parseTreeToAst (Binary bop t1 t2) typBindings = do
  t1' <- parseTreeToAst t1 typBindings
  t2' <- parseTreeToAst t2 typBindings
  return $ App (App (Iden (show bop)) t1') t2'
parseTreeToAst (Unary uop t) typBindings = do
  t' <- parseTreeToAst t typBindings
  return $ App (Iden $ show uop) t'
parseTreeToAst (CaseStmt ptree pcases) typBindings = do
  scrutinee <- parseTreeToAst ptree typBindings
  let maybeCases = map (caseToAst typBindings) pcases
  cases <- safeMaybeToList maybeCases
  return $ Case scrutinee cases
parseTreeToAst (PduConstructor name) _ = Just $ Pdu $ UserType name

-- Take a case clause "K: a1 a2 ... aN -> e" and generate an Ast
-- The type binding contains the type information we need for the ctor.
-- The tuple is (ctorName, listOfParams, expression), relative to the example
-- in the first line of this comment, the tuple is (K,[a1,a2,...,aN],e)
caseToAst :: [TypeBinding] -> (String,[String],PTree)
  -> Maybe (String,[TypeBinding],Term)
caseToAst binding (ctorName, params, ptree) = do
  ctorType <- lookup ctorName binding
  ts <- tryBreakType (length params) ctorType
  -- The pattern is the typed parameter pack that it tries to match
  let pattern = zip params ts
  e <- parseTreeToAst ptree binding
  return (ctorName, pattern, e)

-- converts a function to a top-level abstraction
-- 'name' is only used for errors
-- FIXME Add support for error handling
functionToAbstraction :: TopLevelFunc -> [TypeBinding]-> Maybe (Term,Type)
functionToAbstraction (name, typ, params, body) typBind = do
  inner <- parseTreeToAst body typBind
  typs <- tryBreakType (length params) $ toType typ
  abstract <- makeAbs typs params inner
  return (abstract, toType typ)

functionsToAst :: [TopLevelFunc] -> [TypeBinding] -> Maybe Term
functionsToAst [] _ = Nothing
functionsToAst [func] typBind =
  case functionToAbstraction func typBind of
    Just (expr,_) -> Just expr
    Nothing -> Nothing
functionsToAst (f@(name,_,_,_):funcs) typBind = do
  (expr, resultTyp) <- functionToAbstraction f typBind
  rest <- functionsToAst funcs typBind
  return $ Let name resultTyp expr rest

{-============================================================================-}


-- Type conversion functions
-- These are helper functions that take apart
{-============================================================================-}

filterAdtDecls :: [Declaration] -> [AdtInfo]
filterAdtDecls [] = []
filterAdtDecls [AdtDecl d] = [d]
filterAdtDecls [_] = []
filterAdtDecls ((AdtDecl d):decls) = d:(filterAdtDecls decls)
filterAdtDecls ((_):decls) = filterAdtDecls decls

-- Returns a list of ADT names and a list of ADT constructors ready for gamma
getAdtNamesAndCtors :: [AdtInfo] -> ([String],[TypeBinding])
getAdtNamesAndCtors adts = (names, ctors)
  where names = map adtInfoName adts
        ctors = foldl (++) [] $ map makeADTCtors' adts

makeADTCtors' :: AdtInfo -> [TypeBinding]
makeADTCtors' adtInfo = map makeAdtCtor $ adtInfoCtors adtInfo
  where makeAdtCtor :: (String,[PType]) -> TypeBinding
        makeAdtCtor (ctorName, ptyps) = (ctorName,foldr Func adtType (mt ptyps))
        mt ptyps = map toType ptyps
        adtType = UserType $ adtInfoName adtInfo

filterPduDecls :: [Declaration] -> [PduInfo]
filterPduDecls [] = []
filterPduDecls [PduDecl d] = [d]
filterPduDecls [_] = []
filterPduDecls ((PduDecl d):decls) = d:(filterPduDecls decls)
filterPduDecls ((_):decls) = filterPduDecls decls

-- Returns a list of PDU records and accessors
getPduRecordsAndAccessors :: [PduInfo]
  -> ([(String,[(String,Type)])], [TypeBinding])
getPduRecordsAndAccessors pdus = (records, bindings)
  where
    records = map pduToRecord pdus
    bindings = foldl (++) [] $ map makePduAccessor pdus
    pduToRecord :: PduInfo -> (String,[(String,Type)])
    pduToRecord (PduInfo name fields) = (name,map (\(x,y)->(x,toType y)) fields)

makePduAccessor :: PduInfo -> [TypeBinding]
makePduAccessor (PduInfo n fs) = map makeAccessor fs
  where makeAccessor :: (String,PType) -> TypeBinding
        makeAccessor (fname, ptyp) = (fname, Func (UserType n) (toType ptyp))

{-============================================================================-}



-- Converting PType to Type
{-============================================================================-}
toType :: PType -> Type
toType (PSNat) = SNat
toType (PSChar) = SChar
toType (PSBool) = SBool
toType (PList ptype) = List $ toType ptype
toType (PFunc ptype1 ptype2) = Func t1 t2
  where t1 = toType ptype1
        t2 = toType ptype2
toType (PTVar name) = TVar name
toType (PUserType name) = UserType name
toType (PArray ptype ptree) = Array t e
  where t = toType ptype
        e = case parseTreeToAst ptree [] of
          Just a  -> a
          Nothing -> undefined
toType (PArrayPartial ptype) = ArrayPartial $ toType ptype
toType (PUint ptree1 ptree2) = Uint e1 e2
  where e1 = case parseTreeToAst ptree1 [] of
          Just a  -> a
          Nothing -> undefined
        e2 = case parseTreeToAst ptree2 [] of
          Just a  -> a
          Nothing -> undefined
toType (PUintPartial ptree) =
  UintPartial $
    case parseTreeToAst ptree [] of
      Just a  -> a
      Nothing -> undefined
{-============================================================================-}
