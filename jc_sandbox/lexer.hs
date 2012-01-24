import Char as Char

-- SPL Token types and data structure
-- types: let, =, \, ., <natural number>, <identifier>, ;, (, )
data TokId = LetTok | EqTok | LmTok | DotTok | NatTok | IdTok | SemiTok
            | LParenTok | RParenTok 

-- structure: type, lexeme, line #, column #, filename
data Lexeme = Token TokId String Int Int String

-- simple [0-9] character test
isDigit :: Char -> Bool
isDigit x = Char.isDigit x

-- simple [a-zA-Z_] character test
isAlphaId :: Char -> Bool
isAlphaId '_' = True
isAlphaId x = isAlpha x

-- simple [a-zA-Z_0-9] character test
isAlphaNumId :: Char -> Bool
isAlphaNumId '_' = True
isAlphaNumId x = Char.isAlphaNum x

isLet x = x == "let"
isEq x = x == "="
isLm x = x == "\\"
isDot x = x == "."
isSemi x = x == ";"
isLParen x = x == "("
isRParen x = x == ")"

isKeyword :: String -> Bool
isKeyword x = elem x keywords
               where keywords = [ "let", "=", "\\", "." ]

isSeperator :: Char -> Bool
isSeperator x = elem x seperators || Char.isSpace x
                  where seperators = [ '(', ')', ';' ]

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

tokenize [] = []
            
