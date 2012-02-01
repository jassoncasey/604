import Lexer

-- verify a list of strings against a token class
testTokenCls :: [String] -> TokId -> Bool
testTokenCls (h:tl) symbol =
   let result = tokenizeBuff "" h
   in if isToken result symbol
      then testTokenCls tl symbol
      else False 
testTokenCls [] _ = True

-- verify a set of predefined tests
runTests :: [(String, [String], TokId)] -> Bool
runTests ((tests, symbol):tl) =
   if testTokenCls tests symbol
      then runTests tl
      else False
runTests [] = True

tests = [("010123", NatTok)]

main :: ()
main = do
   if runTests tests
      then putStrLn "Passed lexer tests"
      else putStrLn "Failed lexer tests"
