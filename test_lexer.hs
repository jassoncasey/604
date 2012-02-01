import Lexer
import System

data Status = Success | Failure

-- verify a list of strings against a token class
testTokenCls :: [String] -> TokId -> ( Status, String )
testTokenCls (h:tl) symbol =
   let result = tokenizeBuff "" h
   in if isHeadToken result symbol
      then testTokenCls tl symbol
      else ( Failure, "\tFailed subtest: " ++ h ++ "\n" )
testTokenCls [] _ = ( Success, "" )

-- simple pretty printer for a unit test's status
getTestStr :: String -> Status -> String -> String
getTestStr name result msg =
   case result of 
      Success -> "Passed" ++ boilerplate
      Failure -> "Failed" ++ boilerplate
   where boilerplate = " test: " ++ name ++ "\n" ++ msg

-- runTests :: [( test name, list of tests, expected token id )] -> pass|fail
runTests :: [(String, [String], TokId)] -> ( Status, String )
runTests ((name, tests, symbol):tl) =
   let ( status', msg' ) = (runTests tl)
   in case status of
         Success -> ( status', (getTestStr name status msg) ++ msg' )
         Failure -> ( Failure, (getTestStr name status msg) ++ msg' )
   where ( status, msg ) = testTokenCls tests symbol
runTests [] = ( Success, "Finished ..." )

-- To extend an existing class of tests
-- simpley add another valid input string
-- to the input list of the proper class below
digit_tests = [ "0123456789", "a" ]
id_tests = ["_", "0" ]

-- To extend a class of tests simply add
-- a new tuple to the list that contains:
-- name of class of tests, list of valid
-- strings, and expected lexer token
tests = [("digit", digit_tests, NatTok),
         ("identifier", id_tests, IdTok)]

terminate Success = exitWith ExitSuccess
terminate Failure = exitFailure

main :: IO ()
main = do
   putStrLn ("Starting test runner...\n" ++ msg)
   terminate status
   where ( status, msg ) = runTests tests
