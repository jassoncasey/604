import Parser
import System

data Status = Success | Failure

--runTests :: [] -> ( Status, String )
runTests ((name, tests, symbol):tl) = ( Success, "" )
runTests [] = ( Success, "Finished ..." )

unit_tests = []

-- shell return code handling
terminate :: Status -> IO ()
terminate Success = exitWith ExitSuccess
terminate Failure = exitFailure

-- entry point
main :: IO ()
main = do
   putStrLn ("Starting test runner: parser ...\n" ++ msg)
   terminate status
   where ( status, msg ) = runTests unit_tests
