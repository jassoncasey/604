module Steve.Parser where

{-
  This file defines the external language for Steve and contains an
  implementation of the parser.

  The parser (called top-level parser) is broken into 2 parsers. A data
  structure parser and a function parser. The data structure parser parses,
  you guessed it, data types. The top-level parser then stores type information
  (new user-defined types) and value constructors.

  The other parser is the function parser. It parser annotations and function
  definition. Inside the function body, the expression parser may be called.
-}

{-
  FIXMEs (known bugs)
    - replace your own many and many1 functions with parsecs
    - Types and value constructors need different reserved names lists?
    - integrate new typeExpr function inot code (clean up source code)
    - Use many accum to gather lists
    - correct usedNames list usage
    - add string support
    - put wildcard '_' into case expressions
    - Add bit-string operators
    - Check to make sure there are no name collisions in pdu defs

  ISSUES (known problems)
    - type expressions are NOT intuitive now that we have dependent types
      Example: "Uint 2 a 3" is parsed as "Uint 2 (a 3)"

  INSIGHTS
    - When looking for expression lists, always parse a term. The parser will
      fall through and look for surrounding parantheses or a simple expr and
      then continue on to the rest.
-}

import Steve.Internal

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char (toLower)
import Data.List (sort)




keywords = [
  "Nat","Char","Bool",                -- simple built-in types
  "Uint","Array","Pad",               -- dependent built-in types
  "True", "False",                    -- built-in literals
  "if", "then", "else","case","of",   -- built-in if-then-else support
  "data", "pdu"                       -- Data declarators
  ]

operators = [
  "->", "::",                   -- Annotation
  "*","/","%","+","-",          -- Arithmetic
  "<",">","<=",">=","==","!=",  -- Relational
  "not","or","and",             -- Logical
  ";", ",","."                  -- Delimiters
  ]

languageDef =
  emptyDef{ 
    commentStart = "{-"
  , commentEnd   = "-}"
  , commentLine  = "--"
  , identStart   = lower
  , identLetter  = alphaNum
  , opStart  = oneOf "-:oan><%/*+=!;,."
  , opLetter = oneOf "-:oan><%/*+=!;,."
  , reservedOpNames = operators
  , reservedNames   = keywords
}

TokenParser {
  parens     = m_parens
, lexeme     = m_lexeme
, identifier = m_identifier_simple
, reservedOp = m_reservedOp
, reserved   = m_reserved
, semiSep1   = m_semiSep1
, whiteSpace = m_whiteSpace
, brackets   = m_brackets
, braces     = m_braces
, natural    = m_natural   } = makeTokenParser languageDef




-- Top-Level parser
-- Accepts a list of identifiers that have already been used
-- FIXME Must return list of used names. Otherwise, we can only use parse once.
{-============================================================================-}

parseUnit :: [String] -> Parser [Declaration]
parseUnit usedNames = do
  m_whiteSpace
  declarations <- parseIt usedNames
  eof
  return declarations

-- FIXME This can use manyAccum
parseIt :: [String] -> Parser [Declaration]
parseIt usedNames = (do
    first <- parseDeclaration usedNames
    let newUsedNames = getNamesFromDeclaration first
    if unique newUsedNames
      then do
        rest <- parseIt (newUsedNames ++ usedNames)
        return (first:rest)
      else fail "Names should be unique" )
  <|> return []
  <?> "translation unit"

parseDeclaration :: [String] -> Parser Declaration
parseDeclaration usedNames =
      (dataType usedNames)
  <|> (function usedNames)
  <?> "declaration"

{-============================================================================-}




-- Data type parser
{-
  The data type parser is a bit more complex than the function parser. The data
  type parser must pass back two things:
    1. a new type definition, and
    2. a list of constructors.
  A type definition is a type name and record layout (if record).
  -- FIXME make data type names unique!
-}
{-============================================================================-}

dataType :: [String] -> Parser Declaration
dataType usedNames =
      newPdu usedNames
  <|> adt usedNames
  <?> "data type declaration"

pdu :: [String] -> Parser Declaration
pdu usedNames = (do
    m_reserved "pdu"
    name <-  m_typename
    m_reservedOp "="
    if elem name usedNames
      then fail $ "Redeclaration of \"" ++ name ++ "\""
      else do
        members <- m_braces fieldSequence
        return $ PduDecl $ PduInfo name members--(name, PDUType members)
  ) <?> "pdu type"

fieldSequence :: Parser [(String, PType)]
fieldSequence = (do
    first <- field
    rest  <- manyAccum (:) (m_reservedOp "," >> field)
    return (first:rest)
  ) <?> "fields"

field :: Parser (String, PType)
field = (do
    name <- m_identifier
    m_reservedOp ":"
    typ <- typeExpr
    return (name, typ)
  ) <?> "field"

adt :: [String] -> Parser Declaration
adt usedNames = (do
    m_reserved "data"
    name <- m_typename
    if elem name usedNames
      then fail $ "Redeclaration of \"" ++ name ++ "\""
      else do
        m_reservedOp "="
        constructors <- m_braces constructorSequence
        return $ AdtDecl $ AdtInfo name constructors )
  <?> "adt"

constructorSequence :: Parser [(String, [PType])]
constructorSequence = (do
     first <- constructor
     rest <- manyAccum (:) (m_reservedOp "|" >> constructor)
     return (first:rest) )
  <?> "constructors"

constructor :: Parser (String, [PType])
constructor = (do
    name <- m_typename
    typs <- manyAccum (:) typeTerm
    return (name, typs)
  )
  <?> "value constructor"

newPdu :: [String] -> Parser Declaration
newPdu str = do
    m_reserved "pdu"
    name <- m_typename
    m_reservedOp "="
    definition <- m_braces pduDefinition
    return $ NewPduDecl name definition
  <?> "pdu"

pduDefinition :: Parser [PduPField]
pduDefinition = do
    first <- pduField
    rest <- manyAccum (:) (m_reservedOp "," >> pduField)
    return (first:(reverse rest))
  <?> "pdu definition"

pduField :: Parser PduPField
pduField = padField <|> uintField <|> arrayField <|> depField <?> "pdu field"

padField :: Parser PduPField
padField = do
  m_reserved "Pad"
  expr <- term
  return $ PPadF expr

uintField :: Parser PduPField
uintField = (try $ do
    name <- m_identifier
    m_reservedOp ":"
    t <- uintType
    return $ PNormField name t)
  <?> "uint"

arrayField :: Parser PduPField
arrayField = (try $ do
    name <- m_identifier
    m_reservedOp ":"
    t <- arrayType
    return $ PNormField name t)
  <?> "array"

depField :: Parser PduPField
depField = (try $ do
    name  <- m_identifier
    m_reservedOp ":"
    m_reserved "if"
    b <- term
    m_reserved "then"
    t <- pduType
    return $ PIfThenF name b t )
  <?> "dependent field"


{-============================================================================-}




-- New pdu parser
{-============================================================================-}

{-============================================================================-}



-- Function parser
{-============================================================================-}

-- FIXME Place better requirements on 
function :: [String] -> Parser Declaration
function usedNames = (do
    (name', typeSig) <- functionType
    if elem name' usedNames
      then fail $ "Redeclaration of \"" ++ name' ++ "\""
      else do
        m_reservedOp ";"
        (name, params, body) <- functionDefinition
        m_reservedOp ";"
        if name' == name
          then return $ FuncDecl (name, typeSig, params, body)
          else fail "annotation and definition must have the same name" )
  <?> "function"

--functionType
-- returns the name of the function and the type expression annotating it
functionType :: Parser (String,PType)
functionType = (do
    name <- m_identifier
    m_reservedOp "::"
    typeSig <- typeExpr
    return (name, typeSig)
  ) <?> "type annotation"

functionDefinition :: Parser (String,[String],PTree)
functionDefinition = (do
  name <- m_identifier
  params <- manyAccum (:) m_identifier
  m_reservedOp "="
  body <- expression
  return $ (name, reverse params, body)
  ) <?> "function definition"

{-============================================================================-}




-- Expression Parser
{-============================================================================-}
-- Parsing an expression starts here
expression :: Parser PTree
expression = caseExpression <?> "expression"

caseExpression :: Parser PTree
caseExpression = (do
    m_reserved "case"
    scrutinee <-  expression
    m_reserved "of"
    clauses <- m_braces caseClauseSeq
    return $ CaseStmt scrutinee clauses
  )
  <|> ifExpression
  <?> "case expression"

caseClauseSeq :: Parser [(String,[String],PTree)]
caseClauseSeq = (do
    first <- caseClause
    rest  <- manyAccum (:) (do {m_reservedOp ";"; c <- caseClause; return c})
    return (first:rest)
  )
  <?> "case clause sequence"

caseClause :: Parser (String,[String],PTree)
caseClause = (do 
    ctor <- m_typename
    params <- manyAccum (:) m_identifier
    m_reservedOp "->"
    expr <- caseExpression
    return (ctor,params,expr)
  )
  <?> "case clause"

ifExpression :: Parser PTree
ifExpression = ifThenElse <|> conditionalExpression <?> "expression"

ifThenElse :: Parser PTree
ifThenElse = do
  m_reserved "if"
  b <- expression
  m_reserved "then"
  e1 <- expression
  m_reserved "else"
  e2 <- expression
  return $ IfThenElse b e1 e2
  <?> "conditional expression"

conditionalExpression :: Parser PTree
conditionalExpression =
  buildExpressionParser expressionOperators term <?> "expression"

term :: Parser PTree
term = m_parens expression
  <|> application
  <|> literal

application :: Parser PTree
application = try (do
    iden <- fmap Identifier m_identifier
    firstExpr <- primary -- We require at least one member of an expr list
    restExprs <- manyAccum (:) primary
    let exprList = firstExpr:(reverse restExprs)
    return $ foldl Application iden exprList
  ) <|> projection
  <?> "application"

projection :: Parser PTree
projection = try (do
    expr <- primary
    m_reservedOp "."
    rec <- m_identifier
    return $ Projection expr rec
  ) <|> primary
  <?> "projection"

primary :: Parser PTree
primary = fmap Identifier m_identifier
  <|> literal
  <|> try pduConstructor
  <|> fmap Identifier m_typename  -- Used for constructors
  <?> "primary expression"

literal :: Parser PTree
literal = fmap (\x -> (Literal (LitNat x))) m_natural
  -- <|> character
  -- <|> stringLit
  <|> (m_reserved "True" >> return (Literal (LitBool True)))
  <|> (m_reserved "False" >> return (Literal (LitBool False)))
  <?> "literal"

-- Here is where fact information is found
-- FIXME Not implemented
pduConstructor :: Parser PTree
pduConstructor = (do
    name <- m_typename
    pduRecord <- m_braces pduConstructorBody
    return $ PduConstructor name )
  <?> "pdu record constructor"

pduConstructorBody :: Parser [(String,PTree)]
pduConstructorBody = (do
    first <- pduConstructorField
    rest <- manyAccum (:) (m_reservedOp "," >> pduConstructorField)
    return (first:rest)
  )
  <?> "pdu constructor body"

pduConstructorField :: Parser (String,PTree)
pduConstructorField = (do 
    name <- m_identifier
    m_reservedOp "="
    expr <- expression
    return (name,expr))
  <?> "pdu constructor field"


expressionOperators =
  [
    [prefix "-" Negate], -- hmmm. 1+-2 doesn't parse, but 1+ -2 does (correctly)
    [binaryl "*" Multi, binaryl "/" Div, binaryl "/" Mod],
    [binaryl "+" Plus, binaryl "-" Minus],
    [binaryl "&" BitAnd, binaryl "|" BitOr,
     binaryl "^" BitXor, prefix "~" BitNot],
    [binaryl ">" GreaterThan, binaryl "<" LessThan,
     binaryl ">=" GreaterThanEq, binaryl " <=" LessThanEq],
    [binaryl "==" Equal],
    [prefix "not" Not],
    [binaryl "or" Or, binaryl "and" And]
  ]

{-============================================================================-}




-- Type expression parser
{-============================================================================-}

-- new type expressions
typeExpr :: Parser PType
typeExpr = buildExpressionParser ops typeTerm <?> "type expression"
  where ops = [[ Infix ( m_reservedOp "->" >> return PFunc) AssocRight ]]

typeTerm :: Parser PType
typeTerm = m_parens typeExpr
  <|> listType
  <|> complexType
  <|> (m_reserved "Nat"  >> return PSNat)
  <|> (m_reserved "Bool" >> return PSBool)
  <|> (m_reserved "Char" >> return PSChar)
  <|> fmap PTVar m_identifier
  <?> "type term"

listType :: Parser PType
listType = do
  inner <- m_brackets typeExpr
  return $ PList inner

-- FIXME add support for parametric user types
complexType :: Parser PType
complexType =  parametricType <|> pduType <?> "user defined type"

parametricType :: Parser PType
parametricType = try (do
    name <- m_typename
    params <- manyAccum (:) typeTerm
    -- Params currently unused???
    if null params
      then return $ PUserType name
      else fail "Steve doesn't support parametric ADTs. Yet."
  ) <?> "parametric type"

pduType :: Parser PType
pduType = padType <|> uintType <|> arrayType <?> "pdu type"

padType :: Parser PType
padType = (do
    m_reserved "Pad"
    expr <- term
    return $ PPad expr
  ) <?> "pad type"

uintType :: Parser PType
uintType = (do
    m_reserved "Uint"
    numBits <- primary
    initial <- primary
    return $ PUint numBits initial
  )
  <?> "uint type"

arrayType :: Parser PType
arrayType =  (do
    m_reserved "Array"
    typ <- typeTerm
    expr <- primary
    return $ PArray typ expr
  )
  <?> "array type"

{-padType :: Parser PType
padType = (do
    m_reserved "Pad"
    expr <- term
    return $ Uint (Lit' (LitNat 0)) (Lit' (LitNat 0))
  )
  <?> "pad type"-}

{-============================================================================-}




-- Helper Functions
{-============================================================================-}

-- The following functions construct Prefix and Infix operator values
binary name func assoc =
  Infix (do {m_reservedOp name; return (Binary func)}) assoc
binaryl name func =
  Infix (do {m_reservedOp name; return (Binary func)}) AssocLeft
prefix symbol func = Prefix ( m_reservedOp symbol >> return (Unary func))

-- takes a list of types and turns it into a function type
makeTypeFromList :: [PType] -> PType
makeTypeFromList [] = error "Trying to convert an empty list into a type"
makeTypeFromList [x] = x
makeTypeFromList (x:xs) = PFunc x $ makeTypeFromList xs


-- Extracts all names from a type so that we can ensure that a user doesn't use
-- the same name twice
getNamesFromDeclaration :: Declaration -> [String]
getNamesFromDeclaration (FuncDecl (name,_,_,_)) = [name]
getNamesFromDeclaration (PduDecl pduInfo) = lunzip $ pduInfoFields pduInfo
getNamesFromDeclaration (AdtDecl adtInfo) = lunzip $ adtInfoCtors adtInfo
getNamesFromDeclaration (NewPduDecl n fields) = n : (map getFieldName fields)

-- Handles reserved name errors
isReservedName name = isReserved theReservedNames caseName
  where
    caseName
      | caseSensitive languageDef  = name
      | otherwise = map toLower name

isReserved names name = scan names
  where scan []     = False
        scan (r:rs) =
          case (compare r name) of
            LT  -> scan rs
            EQ  -> True
            GT  -> False
theReservedNames
  | caseSensitive languageDef = sortedNames
  | otherwise = map (map toLower) sortedNames
  where sortedNames = sort (reservedNames languageDef)




-- Improved identifier  parsers
{-============================================================================-}

-- Typenames like in haskell: uppercase letter followed by alphaNum + '_'
typename = 
  m_lexeme $ try $ (do
    c <- upper
    cs <- many (alphaNum <|> char '_')
    let name = (c:cs)
    if isReservedName name
      then unexpected ("reserved name: " ++ name)
      else return name
  )

m_typename = typename <?> "type name"

-- Identifiers are as in haskell: lowercase letter followed by alphaNum + '_'
identifier' = 
    m_lexeme $ try $ (do
    c <- lower
    cs <- many (alphaNum <|> char '_')
    let name = (c:cs)
    if isReservedName name
      then unexpected ("reserved name: " ++ name)
      else return name
  )

m_identifier = identifier' <?> "identifier"

{-============================================================================-}