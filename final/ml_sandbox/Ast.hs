module Steve.Ast where


import Steve.Internal


{-
  The AST file contains all code to convert a parse tree into an AST and to
  convert type declarations into type tables and constructors.
-}

{-
interface:
  getAst
  getTypeTables

-}

-- Process declarations
-- The result of parseUnit is just a declaration list. processDecls converts the
-- list of declarations into a list of top-level functions and a type-table.
{-============================================================================-}

-- takes declarations, returns an AST, list of ADT names, list of PDU records
-- and a list of constructors with which to initialize the environment
processDecls :: [Declaration] ->
                Maybe (Term, [String], [PDURecord], [TypeBinding])
processDecls decls = do
  let udts = filterTypeDeclarations decls
  let funcs = getFuncsFromDecls decls
  let (adtNames, pduRecs, typBindings) = processUserTypes udts
  prgm <- functionsToAst funcs
  return (prgm, adtNames, pduRecs, typBindings)

{-============================================================================-}


-- Function conversion helper functions
-- These functions are reponsible for parts of the top-level function conversion
{-============================================================================-}
getFuncsFromDecls :: [Declaration] -> [TopLevelFunc]
getFuncsFromDecls [] = []
getFuncsFromDecls [TypeDecl _] = []
getFuncsFromDecls [FuncDecl f] = [f]
getFuncsFromDecls ((TypeDecl _):decls) = getFuncsFromDecls decls
getFuncsFromDecls ((FuncDecl f):decls) = f:(getFuncsFromDecls decls)

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
tryBreakType :: Int -> PType -> Maybe [Type]
tryBreakType 0 t = Just [toType t]
tryBreakType n (PFunc t1 t2) =
  case tryBreakType (n-1) t2 of
    Just t  -> Just ((toType t1) : t)
    Nothing -> Nothing
tryBreakType _ t = Nothing

parseTreeToAst :: PTree -> Term
parseTreeToAst (Identifier name) = Iden name
parseTreeToAst (Literal c) = (Lit c)
parseTreeToAst (Application t1 t2) = App (parseTreeToAst t1) (parseTreeToAst t2)
parseTreeToAst (IfThenElse t1 t2 t3) = If t1' t2' t3'
  where t1' = parseTreeToAst t1
        t2' = parseTreeToAst t2
        t3' = parseTreeToAst t3
parseTreeToAst (Binary bop t1 t2) = App (App (Iden(fromBinOpToStr bop)) t1') t2'
  where t1' = parseTreeToAst t1
        t2' = parseTreeToAst t2
parseTreeToAst (Unary uop t) = App (Iden $ fromUnOpToStr uop) $ parseTreeToAst t

-- converts a function to a top-level abstraction
-- 'name' is only used for errors
-- FIXME Add support for error handling
functionToAbstraction :: TopLevelFunc -> Maybe (Term,Type)
functionToAbstraction (name, typ, params, body) = do
  let inner = parseTreeToAst body
  typs <- tryBreakType (length params) typ
  abstract <- makeAbs typs params inner
  return (abstract, toType typ)

functionsToAst :: [TopLevelFunc] -> Maybe Term
functionsToAst [] = Nothing
functionsToAst [func] =
  case functionToAbstraction func of
    Just (expr,_) -> Just expr
    Nothing -> Nothing
functionsToAst (f@(name,_,_,_):funcs) = do
  (expr, resultTyp) <- functionToAbstraction f
  rest <- functionsToAst funcs
  return $ Let name resultTyp expr rest

{-============================================================================-}


-- Type conversion functions
-- These are helper functions that take apart
{-============================================================================-}

filterTypeDeclarations :: [Declaration] -> [UserTypeDef]
filterTypeDeclarations [] = []
filterTypeDeclarations [FuncDecl _] = []
filterTypeDeclarations [TypeDecl t] = [t]
filterTypeDeclarations ((FuncDecl _):decls) = filterTypeDeclarations decls
filterTypeDeclarations ((TypeDecl t):decls) = t : (filterTypeDeclarations decls)

-- Return is, list of ADT names, list of PDU records and a list of Ctors.
-- The Ctors are int a format that can be directly added to the type environment
processUserTypes :: [UserTypeDef] -> ([String],[PDURecord],[TypeBinding])
processUserTypes udts = (adtNames udts, pduRecords udts, makeCtors udts)

adtNames :: [UserTypeDef] -> [String]
adtNames [] = []
adtNames ((n,ADTType _):udts) = n : (adtNames udts)
adtNames (_:udts) = adtNames udts

pduRecords :: [UserTypeDef] -> [PDURecord]
pduRecords [] = []
pduRecords ((n,PDUType rec):udts) = (n,rec) : (pduRecords udts)
pduRecords (_:udts) = pduRecords udts

makeCtors :: [UserTypeDef] -> [TypeBinding]
makeCtors [] = []
makeCtors ((name,ADTType ts):typs) = (makeADTCtors name ts) ++ (makeCtors typs)
makeCtors ((name,PDUType fields):typs) = makeCtors typs


makeADTCtors :: String -> [(String, [PType])] -> [TypeBinding]
makeADTCtors tname ctors =
  case ctors of
    [] -> error "Internal error: ADT defined without constructors."
    [(ctorName,typs)] -> [(ctorName,(makeFunc tname typs))]
    ((ctorName,typs):ctors') ->
      (ctorName,(makeFunc tname typs)):(makeADTCtors tname ctors')
  where makeFunc :: String -> [PType] -> Type
        makeFunc tname' [] = UserType tname
        makeFunc tname' (t:ts) = Func (toType t) $ makeFunc tname' ts


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
        e = parseTreeToAst ptree
toType (PArrayPartial ptype) = ArrayPartial $ toType ptype
toType (PUint ptree1 ptree2) = Uint e1 e2
  where e1 = parseTreeToAst ptree1
        e2 = parseTreeToAst ptree2
toType (PUintPartial ptree) = UintPartial $ parseTreeToAst ptree
{-============================================================================-}
{-============================================================================-}
{-============================================================================-}
{-============================================================================-}
{-============================================================================-}
{-============================================================================-}
{-============================================================================-}







-- Creates a let node for each top level function
{-toAst :: [P.TopLevelFunc] -> Ast
toAst [] = Nil
toAst [func] = toAstFromTop func
toAst (f@(name,_,_,_):funcs) = Let name t $ toAst funcs
  where t = toAstFromTop f

-- Converts a top-level function into an Ast
toAstFromTop :: P.TopLevelFunc -> Ast
toAstFromTop (_, _, args, t) = argsToAbs args t

-- Converts a function body into a lambda calulus AST
argsToAbs :: [String] -> P.PTree -> Ast
argsToAbs [] t = fromPTreeToAst t
argsToAbs [x] t = Abs x $ fromPTreeToAst t
argsToAbs (x:xs) t = Abs x $ argsToAbs xs t

-- Convert a Parse tree to an AST
fromPTreeToAst :: P.PTree -> Ast
fromPTreeToAst (P.Identifier str) = Iden str
fromPTreeToAst (P.Application t1 t2) =
  App (fromPTreeToAst t1) (fromPTreeToAst t2)
fromPTreeToAst (P.IfThenElse t1 t2 t3) =
  If (fromPTreeToAst t1) (fromPTreeToAst t2) (fromPTreeToAst t3)
fromPTreeToAst (P.Literal c) = Lit c
fromPTreeToAst (P.Binary bop t1 t2) =
  App (App (Iden $ fromBinOpToStr bop) (fromPTreeToAst t1)) $ fromPTreeToAst t2
fromPTreeToAst (P.Unary uop t) =
  App (Iden $ fromUnOpToStr uop) (fromPTreeToAst t)-}







