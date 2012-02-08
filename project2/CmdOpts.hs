module CmdOpts
( printUsage
, isSplFile
, verifyArgs
, Mode(..)
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
err_usage :: String
err_usage = "usage error: "

err_bad_ext :: String
err_bad_ext = "One or more files missing .spl file extension."

-- err_file_exist :: String
-- err_file_exist = "One of the specified files does not exist."

usage_syntax :: String
usage_syntax = "usage syntax : ./spl-parser file1.spl ... filen.spl"

-- Error formatting
usage_err :: String -> String
usage_err msg = err_usage ++ msg ++ "\n" ++ usage_syntax ++ "\n"

-- Verifies command arguments
-- The first element of the tuple denotes if the arguments conform to the usage
-- pattern. The second element is an error message if the arguments were invalid
data Mode = Interactive | Batch | Error
verifyArgs :: [String] -> ( Mode, String )
verifyArgs args
   -- no longer true with interpreter mode
   | null args = ( Interactive, "" )
   | not (all isSplFile args) = ( Error, usage_err err_bad_ext )
   -- | not (all splFileExists args) = (False, usage_err err_file_exist )
   | otherwise = ( Batch, "" )


