module Lexer (
   tokenizeLine,  -- String:filename -> Int:lineno -> String:line -> [Token]
   tokenizeBuff,  -- String:filename -> String:buffer -> [Token]
   isToken,       -- Token:lexed token -> TokId:matched id -> Bool
   getLexeme,     -- Token -> String:lexeme
   getErrHdr,     -- Token -> String:formatted lineno/colno
   getFile,
   getLineNo,
   getColStart,
   getColEnd,
   TokId(..),
   Lexeme,
   Token(..)
) where

import Char as Char

-- SPL Token types and data structure
-- types: let, =, \, ., <natural number>, <identifier>, ;, (, )
data TokId = LetTok | EqTok | LamdaTok | DotTok | NatTok | IdTok | SemiTok
           | LParenTok | RParenTok | PlusTok | MinusTok | MultTok | DivTok
           | UnknownTok deriving (Show,Eq)

-- structure: string, line #, column #, filename
data Lexeme = Lex String Int Int String deriving (Show,Eq)

-- structure: type, lexeme
data Token = Tok TokId Lexeme 
            | Empty deriving (Show,Eq)

-- extract the lexeme of a token
getLexeme :: Token -> String
getLexeme token = lexeme
   where (Lexer.Tok _ (Lex lexeme _ _ _)) = token

-- Simple token accessors
getFile :: Token -> String
getFile (Tok _ (Lex _ _ _ fname)) = fname
getLineNo :: Token -> String
getLineNo (Tok _ (Lex _ lineno _ _)) = show lineno
getColStart :: Token -> String
getColStart (Tok _ (Lex _ _ colno _ )) = show colno
getColEnd :: Token -> String
getColEnd (Tok _ (Lex lexeme _ colno _)) = 
   show (colno + (length lexeme) - 1)

-- provide a string containing the relavent info of this token
getErrHdr :: Token -> String
getErrHdr token = 
   "Line: " ++ (show lineno) ++ " " ++
   "Column: " ++  (show colno)
   where (Lexer.Tok _ (Lex lexeme lineno colno fname)) = token

-- simple token length accessor
tokenSize :: Token -> Int
tokenSize (Tok _ ( Lex str _ _ _ ) ) = length str

-- simple token id predicates
isToken :: Token -> TokId -> Bool
isToken (Tok token _ ) match = token == match

fixTokId (Tok a ( Lex str b c d ) ) =
   case str of
      "let" -> Tok LetTok ( Lex str b c d )
      _     -> Tok a ( Lex str b c d )

-- trim the head of the line that matches a character class
trimPatternImp :: String -> String -> String -> ( String, String )
trimPatternImp mtch (hline:tline) ptrn =
   if elem hline ptrn 
   then trimPatternImp (mtch ++ [hline]) tline ptrn
   else ( mtch, (hline:tline) )
trimPatternImp mtch [] _ = ( mtch, [] )

-- generic character class trim function
trimPattern :: String -> String -> (String, String)
trimPattern line pattern = trimPatternImp [] line pattern

-- simple single character operators, keywords, separators
singleChar = [ '(', ')', '.', ';', '\\', '+', '-', '*', '/', '=']
isSingle :: Char -> Bool
isSingle x = elem x singleChar

-- simple identifier predicate
isId :: Char -> Bool
isId x = x == '_' || Char.isAlpha x

-- simple function to create a single character token
tokenizeSingle :: Char -> Int -> Int -> String -> Token
tokenizeSingle ch lineno colno fname = 
   case ch of
      '('   -> Tok LParenTok lex 
      ')'   -> Tok RParenTok lex 
      ';'   -> Tok SemiTok lex
      '='   -> Tok EqTok lex
      '\\'  -> Tok LamdaTok lex
      '.'   -> Tok DotTok lex
      '+'   -> Tok PlusTok lex 
      '-'   -> Tok MinusTok lex
      '*'   -> Tok MultTok lex
      '/'   -> Tok DivTok lex
      where lex = Lex [ch] lineno colno fname

-- tokenize the leading consecutive decimal digits of a string
tokenizeDigits :: String -> Int -> Int -> String -> Token
tokenizeDigits line lineno colno fname = 
   let result = trimPattern line ['0'..'9']
   in Tok NatTok (Lex (fst result) lineno colno fname)

-- tokenize the leading characters that fall within the id class
-- the predicate to get to the is function guards against leading digits
-- if this token is a let it will later be retyped
tokenizeId :: String -> Int -> Int -> String -> Token
tokenizeId line lineno colno fname =
   let cls = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']
       result = trimPattern line cls 
   in Tok IdTok (Lex (fst result) lineno colno fname)

-- tokneize a string into its possible components
tokenizeImp :: String -> Int -> Int -> String -> [Token]
tokenizeImp fname lineno colno (h:tl) 
   -- remove whitespace
   | isSpace h = tokenizeImp fname lineno (colno+1) tl
   -- handle single character keywords and seperators
   | isSingle h = ( tokenizeSingle h lineno colno fname : 
                     tokenizeImp fname lineno (colno+1) tl )
   -- handle natural numbers
   | Char.isDigit h =   let tok = tokenizeDigits (h:tl) lineno colno fname 
                            tok_size = tokenSize tok
                            colno' = colno + tok_size
                        in tok : ( tokenizeImp fname lineno 
                                       colno' ( drop tok_size (h:tl) ) )
   -- handle an identifier or let
   | isId h = let tok = tokenizeId (h:tl) lineno colno fname
                  tok_size = tokenSize tok
                  colno' = colno + tok_size
               in (fixTokId tok) : ( tokenizeImp fname lineno colno' 
                           ( drop tok_size (h:tl) ) )
   -- unknown character in input stream
   | otherwise = tokenizeImp fname lineno (colno+1) tl
tokenizeImp fname lineno colno [] = []

-- tokenize interface
tokenizeLine :: String -> Int -> String -> [Token]
tokenizeLine fname lineno line = tokenizeImp fname lineno 1 line

-- given a filename and list of strings produce a list of tokens
tokenizeBuffImp :: String -> [String] -> Int -> [Token]
tokenizeBuffImp fname (h:tl) lineno = 
   (tokenizeLine fname lineno h) ++ tokenizeBuffImp fname tl (lineno+1) 
tokenizeBuffImp fname [] lineno = []

-- given a filename and a string-buffer produce a list of tokens
tokenizeBuff :: String -> String -> [Token] 
tokenizeBuff fname buff = tokenizeBuffImp fname (lines buff) 1
