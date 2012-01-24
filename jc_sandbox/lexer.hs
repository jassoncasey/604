import Char as Char

-- SPL Token types and data structure
-- types: let, =, \, ., <natural number>, <identifier>, ;, (, )
data TokId = LetTok | EqTok | LamdaTok | DotTok | NatTok | IdTok | SemiTok
            | LParenTok | RParenTok | PlusTok | MinusTok | MultTok | DivTok
            deriving (Show)
-- structure: string, line #, column #, filename
data Lexeme = Lex String Int Int String deriving (Show)
-- structure: type, lexeme
data Token = Tok TokId Lexeme deriving (Show)

tokenSize (Tok _ ( Lex str _ _ _ ) ) = length str

-- simple [0-9] character test
isDigit :: Char -> Bool
isDigit x = Char.isDigit x

digitCls    = ['0'..'9']
lalphaCls   = ['a'..'z']
ualphaCls   = ['A'..'Z']
alphaCls    = lalphaCls ++ ualphaCls
alphaNumCls = digitCls ++ alphaCls
firstIdCls  = alphaCls ++ ['_']
--IdCls       = firstIdCls ++ digitCls

-- simple [a-zA-Z_] character test
isAlphaId :: Char -> Bool
isAlphaId '_' = True
isAlphaId x = isAlpha x

-- simple [a-zA-Z_0-9] character test
isAlphaNumId :: Char -> Bool
isAlphaNumId '_' = True
isAlphaNumId x = Char.isAlphaNum x

-- simple head of line whitespace trim
trim :: String -> String
trim [] = []
trim (h:tl) = if Char.isSpace h then trim tl else h:tl

-- trim the head of the line that matches a character class
trimPatternImpl :: String -> String -> String -> ( String, String )
trimPatternImpl mtch (hline:tline) ptrn =
   if elem hline ptrn 
   then trimPatternImpl (mtch ++ [hline]) tline ptrn
   else ( mtch, (hline:tline) )
trimPatternImpl _ [] _ = ( [], [] )

-- generic character class trim function
trimPattern :: String -> String -> (String, String)
trimPattern line pattern = trimPatternImpl [] line pattern

-- simple single character operators, keywords, separators
singleChar = [ '(', ')', '.', ';', '\\', '+', '-', '*', '/']
isSingle x = elem x singleChar

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

tokenize fname lineno colno (h:tl) 
   | isSpace h = tokenize fname lineno colno+1 tl
   | isSingle h = ( tokenizeSingle h lineno colno fname : 
                     tokenize fname lineno (colno+1) tl )
   | isDigit h = let tok = tokenizeDigit (h:tl) lineno colno fname 
                     colno' = colno + (tokenSize tok)
                  in result : ( tokenize fname lineno colno' )
   | otherwise = [] 
