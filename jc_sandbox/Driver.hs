import System
import Parser

process :: [String] -> IO ()
process ( file : files ) = 
   do buf <- readFile file 
      x <- parse file buf
      process files
process [] = ()

main = do   args <- getArgs
            process args
