module CmdOpts
( printUsage
, isSplFile
, verifyArgs
) where

import Data.Maybe
import Data.List

-- Program usage string
printUsage :: String
printUsage = "usage syntax : ./spl-parser file1.spl ... filen.spl\n"

-- Verifies that the filename string ends with .spl
isSplFile :: String -> Bool
isSplFile filename
   | isJust dot = 
      let n' = fromJust dot + 1 
      in drop n' filename == "spl"
   | otherwise = False
   where dot = findIndex (== '.') filename

-- Basic error strings
err_usage      = "usage error: "
err_no_input   = "no input files."
err_bad_ext    = "One or more files missing .spl file extension."
err_file_exist = "One of the specified files does no    t exist."
usage_syntax   = "usage syntax : ./spl-parser file1.spl ... filen.spl"

-- Error formatting
usage_err :: String -> String
usage_err msg = err_usage ++ msg ++ "\n" ++ usage_syntax ++ "\n"

-- Verifies command arguments
-- The first element of the tuple denotes if the arguments conform to the usage
-- pattern. The second element is an error message if the arguments were invalid
verifyArgs :: [String] -> (Bool,String)
verifyArgs args
   | null args = ( False, usage_err err_no_input )
   | not (all isSplFile args) = ( False, usage_err err_bad_ext )
   -- | not (all splFileExists args) = (False, usage_err err_file_exist )
   | otherwise = ( True, "" )

