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

-- simple token length accessor
tokenSize :: Token -> Int
tokenSize (Tok _ ( Lex str _ _ _ ) ) = length str

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
isSingle :: Char -> Bool
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

-- tokenize the leading consecutive decimal digits of a string
tokenizeDigits :: String -> Int -> Int -> String -> Token
tokenizeDigits line lineno colno fname = 
   let result = trimPattern line ['0'..'9']
   in Tok NatTok (Lex (fst result) lineno colno fname)

-- tokneize a string into its possible compoents
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
   | otherwise = [] 

-- tokenize interface
tokenize :: String -> Int -> String -> [Token]
tokenize fname lineno line = tokenizeImp fname lineno 1 line
