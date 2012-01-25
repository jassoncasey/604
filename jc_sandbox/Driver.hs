import System
import Parser

--process :: [String] -> ()
process ( file : files ) = 
   do buf <- readFile file 
      putStrLn (parse file buf)
      process files
--process [] = ()

main = 
   do args <- getArgs
      process args
