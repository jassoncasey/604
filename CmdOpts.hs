module CmdOpts
( printUsage
, isSplFile
, verifyArgs
) where

import Data.Maybe
import Data.List



-- Program usage string
printUsage :: String
printUsage = "usage: spl-parser file1.spl ... filen.spl\n"



-- Verifies that the filename string ends with .spl
isSplFile :: String -> Bool
isSplFile s
  | isJust n = let n' = fromJust n + 1 in drop n' s == "spl"
  | otherwise = False
  where n = findIndex (== '.') s



-- Verifies command arguments
-- The first element of the tuple denotes if the arguments conform to the usage
-- pattern. The second element is an error message if the arguments were invalid
verifyArgs :: [String] -> (Bool,String)
verifyArgs args
  | null args = (False, "spl-parser error: no input files.\n" ++ printUsage)
  | not (all isSplFile args) = (False, "spl-parser error: One or more files missing .spl file extension.\n" ++ printUsage)
  -- | not (all splFileExists args) = (False, "One of the specified files does not exist.")
  | otherwise = (True, "")