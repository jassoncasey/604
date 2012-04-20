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

import Steve.Internal

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language


keywords = [
  "Nat","Char","Bool",    -- built-in types
  "True", "False",        -- built-in literals
  "if", "then", "else",   -- built-in if-then-else support
  "data", "pdu"           -- Data declarators
  ]

operators = [
  "->", "::",                   -- Annotation
  "*","/","%","+","-",          -- Arithmetic
  "<",">","<=",">=","==","!=",  -- Relational
  "not","or","and",             -- Logical
  ";", ","                      -- Delimiters
  ]

languageDef =
  emptyDef{ 
    commentStart = "{-"
  , commentEnd   = "-}"
  , identStart   = lower
  , identLetter  = alphaNum
  , opStart  = oneOf "-:oan><%/*+=!;,"
  , opLetter = oneOf "-:oan><%/*+=!;,"
  , reservedOpNames = operators
  , reservedNames   = keywords
}

TokenParser {
  parens     = m_parens
, identifier = m_identifier
, reservedOp = m_reservedOp
, reserved   = m_reserved
, semiSep1   = m_semiSep1
, whiteSpace = m_whiteSpace
, brackets   = m_brackets
, braces     = m_braces
, natural    = m_natural   } = makeTokenParser languageDef


data UserDataStructure =
    PDUType [(String,Type)]       -- A name and a list of fields
  | ADTType [(String,Maybe Type)] -- A name and a list of constructors
  deriving (Eq, Show)

type UserType = (String, UserDataStructure)

data Declaration = TypeDecl UserType | FuncDecl TopLevelFunc

-- Top-Level parser
-- Accepts a list of identifiers that have already been used
-- FIXME - add to list of names
{-============================================================================-}
{-parseUnit :: [String] -> Parser [Declaration]
parseUnit usedNames = (do
    typ <- dataType usedNames
    rest <- parse ((getNamesFromUserType typ) ++ usedNames)
    return ((TypeDecl typ):rest)
  ) <|> (do
    fn <- function usedNames
    rest <- parse
    return ((FuncDecl fn):rest)
  ) <|> return []
  <?> "translation unit"-}

parseUnit :: [String] -> Parser [Declaration]
parseUnit usedNames = do
  declarations <- parseIt usedNames
  eof
  return declarations

--FIXME Add support for 
parseIt :: [String] -> Parser [Declaration]
parseIt usedNames = (do
    first <- parseDeclaration usedNames -- FIXME Inlcude names just parsed
    rest <- parseIt ((getNamesFromDeclaration first) ++ usedNames)
    return (first:rest))
  <|> return []
  <?> "translation unit"

parseDeclaration :: [String] -> Parser Declaration
parseDeclaration usedNames = (do
    decl <- dataType usedNames
    return $ TypeDecl decl)
  <|> (do
    decl <- function usedNames
    return $ FuncDecl decl)
  <?> "translation unit"

--parseRest :: [String] -> Parser [Declaration]
--parseRest usedNames



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

dataType :: [String] -> Parser UserType
dataType usedNames =
      pdu usedNames
  <|> adt usedNames
  <?> "data type declaration"

pdu :: [String] -> Parser UserType
pdu usedNames = (do
    m_reserved "pdu"
    name <-  datatype
    if elem name usedNames
      then fail $ "Redeclaration of \"" ++ name ++ "\""
      else do
        members <- m_braces fieldSequence
        if null members
          then fail "Empty pdu declaration"
          else return $ (name, PDUType members)
  ) <?> "pdu type"

fieldSequence :: Parser [(String, Type)]
fieldSequence = (do
    first <- field
    rest  <- fieldSequenceTail
    return (first:rest)
  ) <|> (return [])
  <?> "fields"

fieldSequenceTail :: Parser [(String, Type)]
fieldSequenceTail = (do
    m_reserved ","
    fieldSequence
  ) <|> return []
  <?> "fields"

field :: Parser (String, Type)
field = (do
    name <- m_identifier
    m_reservedOp "="
    typ <- typeExpr
    return (name, typ)
  ) <?> "field"

adt :: [String] -> Parser UserType
adt usedNames = (do
    m_reserved "data"
    name <- datatype
    if elem name usedNames
      then fail $ "Redeclaration of \"" ++ name ++ "\""
      else do
        m_reservedOp "="
        constructors <- m_braces constructorSequence
        return $ (name, ADTType constructors)
  ) <?> "adt"

constructorSequence :: Parser [(String, Maybe Type)]
constructorSequence = (do
    first <- constructor
    rest  <- constructorSequenceTail
    return (first:rest)
  ) <|> return []
  <?> "constructors"

constructorSequenceTail :: Parser [(String, Maybe Type)]
constructorSequenceTail = (do
    m_reserved "|"
    constructorSequence
  ) <|> return []
  <?> "constructors"

constructor :: Parser (String, Maybe Type)
constructor = (do
    name <- datatype
    typ <- typePack
    if null typ
      then return (name, Nothing)
      else return (name, Just $ makeTypeFromList typ)
  )
  <?> "value constructor"

typePack :: Parser [Type]
typePack = (do
    first <- typeExpr
    rest <- typePackTail
    return (first:rest)
  )
  <|> return []
  <?> "type pack"

typePackTail :: Parser [Type]
typePackTail = (do
    m_reserved ","
    typePack
  )
  <|> return []
  <?> "type pack"

datatype :: Parser String
datatype = (do
  first <- upper
  rest <- m_identifier
  return (first:rest))
  <?> "data/constructor type name"

{-============================================================================-}




-- Function parser
{-============================================================================-}

-- FIXME Place better requirements on 
function :: [String] -> Parser TopLevelFunc
function usedNames = (do
    (name', typeSig) <- functionType
    if elem name' usedNames
      then fail $ "Redeclaration of \"" ++ name' ++ "\""
      else do
        m_reservedOp ";"
        (name, params, body) <- functionDefinition
        if name' == name
          then return (name, typeSig, params, body)
          else fail "annotation and definition must have the same name"
  )
  <?> "function"

--functionType
-- returns the name of the function and the type expression annotating it
functionType :: Parser (String,Type)
functionType = (do
    name <- m_identifier
    m_reservedOp "::"
    typeSig <- typeExpr
    return (name, typeSig)
  ) <?> "type annotation"

-- Parse a type expression
-- FIXME this must take a dictionary of known types
typeExpr :: Parser Type
typeExpr = buildExpressionParser ops typeTerm <?> "type expression"
  where ops = [[ Infix ( m_reservedOp "->" >> return Func) AssocRight ]] 

typeTerm :: Parser Type
typeTerm = m_parens typeExpr
  <|> listType
  <|> fmap TVar m_identifier
  <|> (m_reserved "Nat"  >> return SNat)
  <|> (m_reserved "Bool" >> return SBool)
  <|> (m_reserved "Char" >> return SChar)
  <?> "type term"

listType :: Parser Type
listType = do
  inner <- m_brackets typeExpr
  return $ List inner

functionDefinition :: Parser (String,[String],PTree)
functionDefinition = (do
  name <- m_identifier
  params <- getParams
  m_reservedOp "="
  body <- expression
  return $ (name, params, body)
  ) <?> "function definition"

getParams :: Parser [String]
getParams = (do
    first <- m_identifier
    rest <- getParams
    return (first:rest)
  ) <|> (return [])
  <?> "parameters"

{-============================================================================-}




-- Expression Parser
{-============================================================================-}

expression :: Parser PTree
expression = ifThenElse <|> conditionalExpression <?> "expression"

conditionalExpression :: Parser PTree
conditionalExpression =
  buildExpressionParser expressionOperators term <?> "expression"

term :: Parser PTree
term = m_parens expression
  <|> application
  <|> literal

application :: Parser PTree
application = (do
    pack <- parameterPack
    return $ makeApplication pack
  ) <|> primary
  <?> "application"

-- Fails if there is no paramter pack
parameterPack :: Parser [PTree]
parameterPack = do
  first <- primary
  rest <- restOfParameterPack
  return (first:rest)
  where restOfParameterPack :: Parser [PTree]
        restOfParameterPack = do
          parameterPack <|> (return [])

primary :: Parser PTree
primary = fmap Identifier m_identifier
  <|> literal
  <?> "primary expression"

literal :: Parser PTree
literal = fmap (\x -> (Literal (LitNat x))) m_natural
  <|> (m_reserved "True" >> return (Literal (LitBool True)))
  <|> (m_reserved "False" >> return (Literal (LitBool False)))
  <?> "literal"

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

expressionOperators =
  [
    [prefix "-" Negate], -- hmmm. 1+-2 doesn't parse, but 1+ -2 does (correctly)
    [binaryl "*" Multi, binaryl "/" Div, binaryl "/" Mod],
    [binaryl "+" Plus, binaryl "-" Minus],
    [binaryl ">" GreaterThan, binaryl "<" LessThan,
     binaryl ">=" GreaterThanEq, binaryl " <=" LessThanEq],
    [prefix "not" Not],
    [binaryl "or" Or, binaryl "and" And]
  ]

-- Takes a [PTree] string and turns it into an application tree
makeApplication :: [PTree] -> PTree
makeApplication [] = error "Calling makeApplication on an empty list of nodes."
makeApplication [n] = n
makeApplication ns = Application (makeApplication $ init ns) $ last ns

{-============================================================================-}



-- Helper Functions
{-============================================================================-}

-- The following functions construct Prefix and Infix operator values
binary name func assoc =
  Infix (do {m_reservedOp name; return (Binary func)}) assoc
binaryl name func =
  Infix (do {m_reservedOp name; return (Binary func)}) AssocLeft
prefix symbol func = Prefix ( m_reservedOp symbol >> return (Unary func))


-- Removes the \" characters from strings
cleanDQ :: String -> String
cleanDQ "" = ""
cleanDQ ('\\':str) = cleanDQ str
cleanDQ ('\"':str) = cleanDQ str
cleanDQ (a:str)    = a : (cleanDQ str)


testParser :: String -> String
testParser input = case parse expression "steve" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

-- takes a list of types and turns it into a function type
makeTypeFromList :: [Type] -> Type
makeTypeFromList [] = error "Trying to convert an empty list into a type"
makeTypeFromList [x] = x
makeTypeFromList (x:xs) = Func x $ makeTypeFromList xs

-- Extracts all names from a type so that we can ensure that a user doesn't use
-- the same name twice
getNamesFromDeclaration :: Declaration -> [String]
getNamesFromDeclaration (TypeDecl (name, userType)) = (name : memberNames)
  where memberNames = wertfgh userType
getNamesFromDeclaration (FuncDecl (name,_,_,_)) = [name]

wertfgh :: UserDataStructure -> [String]
wertfgh a = case a of
  PDUType fields       -> lunzip fields
  ADTType constructors -> lunzip constructors

lunzip :: [(a,b)] -> [a]
lunzip [] = []
lunzip [(x,_)] = [x]
lunzip ((x,_):x_s) = x : (lunzip x_s)