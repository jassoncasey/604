import System
import Lexer

-- test takes a binary predicate and two lists and returns 
-- a full binary predicate test
test :: (Eq a) => ( a -> a -> Bool ) -> [a] -> [a] -> Bool
test bpredicate (h:tl) (h':tl') 
  | bpredicate h h' = test bpredicate tl tl'
  | otherwise = False
test bpredicate [] [] = True
test bpredicate [] _ = False
test bpredicate _ [] = False

-- generic test given fixed input
testEq :: String -> [Token] -> Bool
testEq input output func =
   let result = func input
   test (==) output result 

foreach (h:tl) pred
   | pred h = foreach tl pred
   | otherwise = False
foreach [] _ = True

test_digit =
   let digits = [0..9]
   in foreach 

main = 
   let tests = []
   in foreach 
   if test (==) [1,2,3,4] [1,2,3,4]
   then putStrLn "yes"
   else putStrLn "no"
