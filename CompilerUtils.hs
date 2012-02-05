module CompilerUtils
(
   minFreeString,
   Tree(..),
   buildTreeStr,
   mergeItem,
   mergeList,
   mergeTree,
   buildTree
) where

import Data.Char

data Tree = 
   Interior Char [Tree]
   | Root [Tree]
   deriving (Eq,Show)

-- charater classes to search
first_char :: String
first_char = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
other_char :: String
other_char = first_char ++ ['0'..'9']

minFreeList :: [Tree] -> String
minFreeList ((Interior val children):tl) = ""
    
minFree :: Tree -> String
minFree (Root children) = minFreeList children
minFree (Interior val (h:tl)) = ""
minFree (Interior val []) = other_char

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
   "z"
   where tree = buildTreeStr inputs
