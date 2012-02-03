import System
import Parser
import ParseTree

-- take a filename and make sure it is a *.spl format
badPostFix :: String -> Bool
badPostFix name = 
   (length name) < 5 || ( take 4 (reverse name) ) /= "lps."

-- take a list of filenames and verify they are not incorrect
badPostFixes :: [String] -> Bool
badPostFixes (h:tl) = badPostFix h || badPostFixes tl
badPostFixes [] = False

-- Verify at least 1 input file is given and they are *.spl files
badUsage :: [String] -> Bool
badUsage args = 
   length args < 1 || badPostFixes args

--process :: [String] -> ()
process ( file : files ) = 
   do buf <- readFile file 
      putStrLn ("Compiling: " ++ file)
      let prog = parse file buf
      putStrLn "-------------------------------\n"
      putStrLn (ParseTree.getStrProgram prog)
      putStrLn "-------------------------------\n"
      process files
process [] = return () 

-- repeat the processing for each argument
main :: IO ()
main = 
   do args <- getArgs
      if badUsage args 
         then putStrLn "usage syntax: ./splc <input>.spl+"
         else process args
