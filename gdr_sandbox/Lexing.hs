-- Copyright (C) 2012, Texas A&M University.  All rights reserved.
-- Written by Gabriel Dos Reis.

module Lexing (Token (..), TokenType(..), Key(..), 
               tokenize, token_type, lexString) where
import Char

-- return an action that will break then content of the
-- the input file into a sequence of tokens.
--lex_file :: FilePath -> IO [Token]
--lex_file f = do
-- chars <- readFile f
--  return $ tokenize (mk_source_locus f 1 0) chars

-- MJ Edit
-- return the tokens from the source of a file
lexString :: String -> String -> [Token]
lexString fname src = tokenize (mk_source_locus fname 1 0) src


-- break an input string into a list of tokens, with a specified
-- starting source locus
tokenize :: SourceLocation -> [Char] -> [Token]
tokenize sl text =
  go sl text [] where
    go sl' text'' ts =
      case next_token text'' sl' of
         Nothing -> reverse ts
         Just (t, text', sl'') -> go sl'' text' (t:ts)

-- The main worker function of the lexer sub-component.
next_token :: [Char] -> SourceLocation -> LexerResult
next_token s sl =
  case s of
    [] -> Nothing

    -- separators
    '\n':cs -> next_token cs (lineno_inc sl)
    c:cs | is_nonblank_sep c -> 
      Just (Token (Separator c) [c] sl, cs, colno_inc sl)

    -- keywords
    '=':cs -> Just (Token (Keyword EqKey) "=" sl, cs, colno_inc sl)
    '\\':cs -> Just (Token (Keyword LambdaKey) "\\" sl, cs, colno_inc sl)
    '.':cs -> Just (Token (Keyword PeriodKey) "." sl, cs, colno_inc sl)

    -- natural
    c:_ | Char.isDigit c -> parse_integer s sl

    -- operators
    c:cs | is_binop c -> Just (Token (Operator [c]) [c] sl, cs, colno_inc sl)

    -- identifiers
    c:_ | is_letter c -> parse_identifier s sl

    -- junk
    c:cs | is_blank c -> next_token cs (colno_inc sl)
    _ -> skip_junk s sl

parse_integer :: String -> SourceLocation -> LexerResult
parse_integer s sl =
  let (i,s') = span isDigit s
  in Just (Token (Natural $ read i) i sl, s', colno_add sl (length i))

parse_identifier :: String -> SourceLocation -> LexerResult
parse_identifier s sl =
  let (i,s') = span is_id_part s where
                 is_id_part c = is_letter c || isDigit c
      tt = if i == "let" then Keyword LetKey else Identifier
  in Just (Token tt i sl, s', colno_add sl (length i))

skip_junk :: String -> SourceLocation -> LexerResult
skip_junk s sl =
  let (s',s'') = break is_separator s
  in Just (Token Junk s' sl, s'', colno_add sl (length s'))

-- An input source locations is a triple identifying the location of
-- an lexical entity in the input source program.
data SourceLocation = 
  SourceLocation {
    input_file :: FilePath,    -- file name
    line_number :: Int,        -- line number
    column_number :: Int       -- column number
  }
  --deriving Show

quote :: String -> String
quote s = "\"" ++ s ++ "\""

instance Show SourceLocation where
  show SourceLocation { input_file = f, line_number = l, column_number = c } =
    "{file =  " ++ quote f 
      ++ ", line =  " ++ show l 
        ++ ", column = " ++ show c ++ "}"

-- The result of a one-step lexing is either nothing (in case the
-- input is exhausted) or some token, the remainng text to process,
-- and an update source location.
type LexerResult = Maybe (Token, [Char], SourceLocation)

data Token = 
  Token TokenType String SourceLocation

token_type :: Token -> TokenType
token_type (Token tt _ _) = tt

instance Show Token where
  show (Token _ s _) = 
    show s

-- In addition to the token type listed in assignment 1, we have
-- `Operator' for binary operator, `Junk' for anything else.
data TokenType =
    Junk                      -- unidentified flying object
  | Identifier                -- ascii identifiers
  | Operator String           -- binary arithmetic ops
  | Keyword Key               -- keywords
  | Natural Integer           -- natural numbers
  | Separator Char            -- separators
  deriving (Eq,Show)

-- SourceLocation `smart' constructor, for convenience
mk_source_locus :: FilePath -> Int -> Int -> SourceLocation
mk_source_locus f l c =
  SourceLocation { input_file = f, line_number = l, column_number = c }

lineno_inc :: SourceLocation -> SourceLocation
lineno_inc sl = 
  mk_source_locus (input_file sl) (1 + line_number sl) (column_number sl)

colno_inc :: SourceLocation -> SourceLocation
colno_inc sl =
  mk_source_locus (input_file sl) (line_number sl) (1 + column_number sl)

colno_add :: SourceLocation -> Int -> SourceLocation
colno_add sl n = 
  mk_source_locus (input_file sl) (line_number sl) (column_number sl + n)

data Key =
    LetKey             -- "let"
  | EqKey              -- "="
  | LambdaKey          -- "\"
  | PeriodKey          -- "."
  deriving (Eq,Show)

is_binop :: Char -> Bool
is_binop c =
  elem c "*/+-"

is_letter :: Char -> Bool
is_letter c =
   isAlpha c || c == '_'

is_paren :: Char -> Bool
is_paren c =
  c == '(' || c == ')'

is_nonblank_sep :: Char -> Bool
is_nonblank_sep c =
  is_paren c || c == ';'

is_blank :: Char -> Bool
is_blank c =
  c == ' ' || c == '\n'

is_separator :: Char -> Bool
is_separator c =
  is_blank c || is_nonblank_sep c

