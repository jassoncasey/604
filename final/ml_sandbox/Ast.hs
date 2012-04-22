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
-- output: (a,b,c)
--   a : list of top level functions (parse tree)
--   b : list of type names (symbols for lookup)
--   c : pdu definitions
--   d : value constructors
--processDecls :: [Declaration]
--                -> ([TopLevelFunc], [String], [PDURecord], [(String,Type)])
--processDecls decls = (getFuncsFromDecls decl, )


{-getFuncsFromDecls :: [Declaration] -> [TopLevelFunc]
getFuncsFromDecls [] = []
getFuncsFromDecls [TypeDecl _] = []
getFuncsFromDecls [FuncDecl f] = [f]
getFuncsFromDecls ((TypeDecl _):decls) = getFuncsFromDecls decls
getFuncsFromDecls ((FuncDecl f):decls) = f:(getFuncsFromDecls decls)

getTypesFromDecls :: [Declaration] -> [UserType]
getTypesFromDecls [] = []
getTypesFromDecls [TypeDecl t] = [t]
getTypesFromDecls [FuncDecl _] = []
getTypesFromDecls ((TypeDecl t):decls) = t:(getFuncsFromDecls decls)
getTypesFromDecls ((FuncDecl _):decls) = getFuncsFromDecls decls

processType :: UserType -> (String, Maybe PDURecord, [(String,Type)])
processType (name, PDUType fields) = (name, Just (name, fields), ctor)
  where ctor = makePDUCtor ctor
processType (name, ADTType ctors) = (name, Nothing, processADTCtors ctors)-}

--makePDUCtor :: String -> [(String,Type)] -> (String, [Types])
--makePDUCtor name fields


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
makeAbs [] [] e = Just e
makeAbs _  [] _ = Nothing
makeAbs [] _  _ = Nothing
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
functionToAbstraction :: TopLevelFunc -> Maybe Term
functionToAbstraction (name, typ, params, body) = do
  let inner = parseTreeToAst body
  typs <- tryBreakType (length params) typ
  abstract <- makeAbs typs params inner
  return abstract

toAst :: [TopLevelFunc] -> Maybe Term
toAst [] = Nothing
toAst [func] = functionToAbstraction func
toAst (f@(name,typ,_,_):funcs) = do
  expr <- functionToAbstraction f
  rest <- toAst funcs
  return $ Let name typ expr rest

{-============================================================================-}



{-============================================================================-}
{-============================================================================-}

{-============================================================================-}
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










-- fromBinaryOp to String
fromBinOpToStr :: BinOp -> String
fromBinOpToStr Plus          = "+"
fromBinOpToStr Minus         = "-"
fromBinOpToStr Multi         = "*"
fromBinOpToStr Div           = "/"
fromBinOpToStr Mod           = "%"
fromBinOpToStr LessThan      = "<"
fromBinOpToStr LessThanEq    = "<="
fromBinOpToStr GreaterThan   = ">"
fromBinOpToStr GreaterThanEq = ">="
fromBinOpToStr Equal         = "="
fromBinOpToStr NotEqual      = "<>"
fromBinOpToStr Or            = "or"
fromBinOpToStr And           = "and"

fromUnOpToStr :: UnOp -> String
fromUnOpToStr Not = "not"
fromUnOpToStr Negate = "-"
