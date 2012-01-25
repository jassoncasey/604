import System
import Parser

-- take a filename and make sure it is a *.spl format
badPostFix :: String -> Bool
badPostFix name = 
   ((length name) < 5) || (( take 4 (reverse name) ) /= "lps.")

-- take a list of filenames and verify they are not incorrect
badPostFixes :: [String] -> Bool
badPostFixes (h:tl) = badPostFix || badPostFixes tl
badPostFixes [] = False

-- Verify at least 1 input file is given and they are *.spl files
badUsage :: [String] -> Bool
badUsage args = 
   show length args < 1 || badPostFixes args

--process :: [String] -> ()
process ( file : files ) = 
   do buf <- readFile file 
      putStrLn (parse file buf)
      process files
-- what is the proper way to write the terminating portion
--process [] = () 

-- repeat the processing for each argument
main :: IO ()
main = 
   do args <- getArgs
      if badUsage args 
      then putStrLn "usage error: ./splc <input>.spl+"
      else process args
