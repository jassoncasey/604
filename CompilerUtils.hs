module CompilerUtils
(
   minFreeString,
) where

import Data.Char

data Tree = 
   Interior Char [Tree]
   | Root [Tree]
   deriving (Eq,Show)

-- charater classes to search
universe :: String
universe = ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ ['0'..'9']

-- find the set difference with the current
getPossible :: [Tree] -> String
getPossible ((Interior val _):tl) =
   filter (/=val) (getPossible tl) 
getPossible _ = universe

-- simple value accessor
getVal :: Tree -> String
getVal (Interior val _ ) = [val]
getVal _ = "_"

-- extract the minimum string not present in the tree
getMinTree :: Tree -> String
getMinTree (Root (h:tl)) =
   if length possible == 0
      then (getVal h) ++ (getMinTree h)
      else [head possible]
   where possible = getPossible (h:tl)
getMinTree (Interior _ (h:tl)) = 
   if length possible == 0
      then (getVal h) ++ (getMinTree h)
      else [head possible]
   where possible = getPossible (h:tl)
getMinTree _ = "_z_"

-- merge a single item into a list of items
mergeItem :: [Tree] -> Tree -> [Tree]
mergeItem ((Interior val children):tl) (Interior val' children') =
   if val == val'
      then ((Interior val (mergeList children children')):tl)
      else ((Interior val children):(mergeItem tl (Interior val' children')))
mergeItem [] x = [x]
mergeItem _ _ = []

-- merge two lists of trees
mergeList :: [Tree] -> [Tree] -> [Tree]
mergeList nodes (h:tl) =
   mergeList (mergeItem nodes h) tl
mergeList nodes [] = nodes

-- merge two trees
mergeTree :: Tree -> Tree -> Tree
mergeTree (Interior val left) (Interior _ right) = Interior val (mergeList left right)
mergeTree (Root left) (Root right) = Root (mergeList left right)
mergeTree _ _ = Root []

-- build a tree from a string
buildTree :: String -> Tree
buildTree (h:tl) =
   if length tl == 0
      then Interior h []
      else Interior h [(buildTree tl)]
buildTree [] = Root []

-- build a tree from a list of strings
buildTreeStr :: [String] -> Tree
buildTreeStr (h:tl) =
   mergeTree (Root [buildTree h]) (buildTreeStr tl)
buildTreeStr [] = Root []

-- find the minimum free string
minFreeString :: [String] -> String
minFreeString inputs =
   getMinTree (buildTreeStr inputs)
