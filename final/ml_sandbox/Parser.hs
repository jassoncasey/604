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

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

-- Parse tree structure
data PTree =
    Identifier String
  | Function String Types [String] PTree
  | Application PTree PTree
  | IfThenElse PTree PTree PTree
  | Binary BinOp PTree PTree
  | Unary UnOp PTree
  | Literal Constant deriving (Eq, Show)

data Constant = 
   LitBool Bool
 | LitInt Integer deriving (Eq, Show)

-- Binary Operators
data BinOp =
    Plus | Minus | Multi | Div | Mod                    -- Arithmetic
  | LessThan | LessThanEq | GreaterThan | GreaterThanEq -- Ordering
  | Equal | NotEqual                                    -- Equality
  | Or | And deriving (Eq, Show)                        -- Logical

-- Unary Operators
data UnOp = Not | Negate deriving (Eq, Show)

type TopLevelFunc = (String, Types, [String], PTree)

-- Types
data Types =
    TypeVar String
  | TNat
  | TBool
  | TChar
  | Func Types Types
  | List Types
  | Inductive
  | Record deriving (Eq, Show)

keywords = [
  "Int","Char","Bool",    -- built-in types
  "True", "False",        -- built-in literals
  "if", "then", "else"    -- built-in if-then-else support
  ]

operators = [
  "->", "::",                   -- Annotation
  "*","/","%","+","-",          -- Arithmetic
  "<",">","<=",">=","==","!=",  -- Relational
  "not","or","and",             -- Logical
  ";"
  ]

languageDef =
  emptyDef{ 
    commentStart = "{-"
  , commentEnd   = "-}"
  , identStart   = lower
  , identLetter  = alphaNum
  , opStart  = oneOf "-:oan><%/*+=!;"
  , opLetter = oneOf "-:oan><%/*+=!;"
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
, natural    = m_natural   } = makeTokenParser languageDef



-- Top-Level parser
{-============================================================================-}



{-============================================================================-}




-- Data type parser
{-============================================================================-}



{-============================================================================-}




-- Function parser
{-============================================================================-}

-- FIXME Place better requirements on 
function :: Parser TopLevelFunc
function = (do
    (_, typeSig) <- functionType
    m_reservedOp ";"
    (name, params, body) <- functionDefinition
    return (name, typeSig, params, body))
  <?> "function"

--functionType
-- returns the name of the function and the type expression annotating it
functionType :: Parser (String,Types)
functionType = (do
    name <- m_identifier
    m_reservedOp "::"
    typeSig <- typeExpr
    return (name, typeSig)
  ) <?> "type annotation"

-- Parse a type expression
-- FIXME this must take a dictionary of known types
typeExpr :: Parser Types
typeExpr = buildExpressionParser ops typeTerm <?> "type expression"
  where ops = [[ Infix ( m_reservedOp "->" >> return Func) AssocRight ]] 

typeTerm :: Parser Types
typeTerm = m_parens typeExpr
  <|> listType
  <|> fmap TypeVar m_identifier
  <|> (m_reserved "Nat"  >> return TNat)
  <|> (m_reserved "Bool" >> return TBool)
  <|> (m_reserved "Char" >> return TChar)
  <?> "type term"

listType :: Parser Types
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
literal = fmap (\x -> (Literal (LitInt x))) m_natural
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
testParser input = case parse function "steve" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val