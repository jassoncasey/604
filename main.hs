import System( getArgs)
import System.IO
import Tokenizer
import TokenizerPrint

f :: [Int] -> Int
f [] = 0
f (2:xs) = 3
f (_:xs) = 1


main = do
  args <- getArgs
  -- needs if to guard against empty
  content <- readFile $ head args
  print $ tokenize (head args) 21 content