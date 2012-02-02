module Environment
( Env(..)
, lookUp
, bind
, pushScope
, popScope
) where

import Data.Map as Map
import Data.Maybe



data Env k a = Env [(Map k a)] | Tip deriving (Show)

-- Performs lookup on maps starting with the top scope and descending
lookUp :: (Ord k) => Env k a -> k -> Maybe a
lookUp Tip _ = Nothing
lookUp (Env (c:cs)) k =
  case result of
    Nothing -> lookUp (Env cs) k
    Just a  -> Just a
  where result = Map.lookup k c
lookup a b = error (show a)

-- Bind an identifier to the current scope
bind :: (Ord k) => Env k a -> k -> a -> Env k a
bind Tip k a = Env [(insert k a Map.empty)]
bind (Env (c:cs)) k a = (Env ((insert k a c):cs))

-- Add a new scope to the environment
pushScope :: Env k a -> Env k a
pushScope Tip = Env [Map.empty]
pushScope (Env k) = Env (Map.empty : k)

-- Remove current scope from the environment
popScope :: Env k a -> Env k a
popScope Tip = Tip
popScope (Env (m:ms)) = Env ms
