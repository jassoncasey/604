import Lexer

-- verify a list of strings against a token class
testTokenCls :: [String] -> TokId -> Bool
testTokenCls (h:tl) symbol =
   let result = tokenizeBuff "" h
   in if isHeadToken result symbol
      then testTokenCls tl symbol
      else False 
testTokenCls [] _ = True

-- runTests :: [( test name, list of tests, expected token id )] -> pass|fail
runTests :: [(String, [String], TokId)] -> Bool
runTests ((name, tests, symbol):tl) =
   if testTokenCls tests symbol
      then runTests tl
      else False
runTests [] = True

-- To extend an existing class of tests
-- simpley add another valid input string
-- to the input list of the proper class below
digit_tests = [ "0123456789" ]
id_tests = ["_"]

-- To extend a class of tests simply add
-- a new tuple to the list that contains:
-- name of class of tests, list of valid
-- strings, and expected lexer token
tests = [("digit", digit_tests, NatTok),
         ("identifier", id_tests, IdTok)]

main :: IO ()
main = do
   if runTests tests
      then putStrLn "Passed lexer tests"
      else putStrLn "Failed lexer tests"
