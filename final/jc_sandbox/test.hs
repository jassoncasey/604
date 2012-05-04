import System
import System.IO
import qualified Data.ByteString as B

blen :: IO B.ByteString -> Int
blen b = B.length b

main :: IO ()
main = do
   args <- getArgs
   putStrLn $ "Opening: " ++ ( head args )
   h <- openBinaryFile ( head args ) ReadMode 
   buf <- hGetContents h
   hClose h
   putStrLn $ blen buf
