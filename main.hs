import System( getArgs)
import System.IO
import Tokenizer
import Data.Maybe as Maybe
import Data.List as List
import TokenizerPrint

-- UNSAFE
breakAfter :: (a -> Bool) -> [a] -> ([a],[a])
breakAfter _ [] = ([],[])
breakAfter p l
  | isJust n = let n' = fromJust n + 1 in (take n' l, drop n' l)
  | otherwise = (l,[])
  where n = findIndex p l

-- Doesn't remove empty lists
splitAfterImpl :: (a -> Bool) -> [a] -> [[a]]
splitAfterImpl p s =
  let (l, s') = breakAfter p s
  in l : case s' of
    [] -> []
    (_: s'') -> splitAfterImpl p s''

splitAfter :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
splitAfter p l = filter (not . (==[])) $ splitAfterImpl p l

isSplFile :: String -> Bool
isSplFile s
  | isJust n = let n' = fromJust n + 1 in drop n' s == "spl"
  | otherwise = False
  where n = findIndex (== '.') s

verifyArgs :: [String] -> Bool
verifyArgs l = all isSplFile l

main = do
  args <- getArgs
  -- needs if to guard against empty
  --content <- readFile $ head args
  if null args
    then putStr printUsage
    else
      if not (all isSplFile args)
        then putStr printUsage
        else
          if False -- FIXME One of the files does not exist
            then
              putStrLn "One of the files specified does not exist" -- find which one
            else
              -- get contents from each file
              return ()
  -- verify that there are arguments
  -- verify that each argument is an spl file
  --print $ tokenize (head args) 21 content