module ListAux( splitAfter ) where

import Data.Maybe as Maybe
import Data.List as List

-- breakAfter works like list, except it includes the first element in the list
-- that doesn't satisfy the predicate
breakAfter :: (a -> Bool) -> [a] -> ([a],[a])
breakAfter _ [] = ([],[])
breakAfter p l
  | Maybe.isJust n = let n' = fromJust n + 1 in (take n' l, drop n' l)
  | otherwise = (l,[])
  where n = findIndex p l


-- splitAfter splits a list when it encounters an element that does not satisfy
-- the predicate and includes the predicate with the preceding elements
-- Example splitAfter (< 3) [1,2,3,1,2,4,2,4] = [[1,2,3],[1,2,4],[2,4]]
splitAfter :: (a -> Bool) -> [a] -> [[a]]
splitAfter p l = filter (not . null) $ splitAfterImpl p l

splitAfterImpl :: (a -> Bool) -> [a] -> [[a]]
splitAfterImpl p s =
  let (l, s') = breakAfter p s
  in l : case s' of
    [] -> []
    (_: s'') -> splitAfterImpl p s''