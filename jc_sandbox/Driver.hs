import System
import Parser

badPostFix name = 
   show length name < 5 || ( take 4 (reverse name) ) != "lps."

badPostFixes (h:tl) = badPostFix || badPostFixes tl
badPostFixes [] = False

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
