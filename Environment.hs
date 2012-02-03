module Environment
( Env(..)
, lookUp
, bind
, pushScope
, popScope
) where

import qualified Data.Map as Map
import Data.Maybe()

import Ast



data Env = Env [(Map.Map String Expression)] | Tip deriving (Show)

-- Performs lookup on maps starting with the top scope and descending
lookUp :: Env -> String -> Maybe Expression
lookUp Tip _ = Nothing
lookUp (Env []) _ = Nothing
lookUp (Env (c:cs)) k =
  case result of
    Nothing -> lookUp (Env cs) k
    Just a  -> Just a
  where result = Map.lookup k c

-- Bind an identifier to the current scope
bind :: Env -> String -> Expression -> Env
bind Tip k a = Env [(Map.insert k a Map.empty)]
bind (Env []) k a = Env [(Map.insert k a Map.empty)]
bind (Env (c:cs)) k a = (Env ((Map.insert k a c):cs))

-- Add a new scope to the environment
pushScope :: Env -> Env
pushScope Tip = Env [Map.empty]
pushScope (Env k) = Env (Map.empty : k)

-- Remove current scope from the environment
popScope :: Env -> Env
popScope Tip = Tip
popScope (Env []) = Env []
popScope (Env (_:ms)) = Env ms

