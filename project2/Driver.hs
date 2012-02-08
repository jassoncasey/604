import System
import System.IO
import Parser
import qualified ParseTree as PT
import qualified Ast as Ast
import qualified AstPrime as AstPrime
import qualified EvalAstPrime as EAP

-- take a filename and make sure it is a *.spl format
badPostFix :: String -> Bool
badPostFix name = 
   (length name) < 5 || ( take 4 (reverse name) ) /= "lps."

-- take a list of filenames and verify they are not incorrect
badPostFixes :: [String] -> Bool
badPostFixes (h:tl) = badPostFix h || badPostFixes tl
badPostFixes [] = False

-- Verify at least 1 input file is given and they are *.spl files
badUsage :: [String] -> Bool
badUsage args = 
   length args > 1 && badPostFixes args

process :: String -> String -> IO ()
process filename buf = do
   putStrLn ("Compiling: " ++ filename)
   let prog = parse filename buf
   let ast = Ast.transformProg prog
   let ast' = AstPrime.transformAst ast
   let result = EAP.eval ast'
   putStrLn "--------------Parse Tree-----------------"
   putStr (PT.getStrProgram prog)
   putStrLn "------------------AST--------------------"
   putStr ( Ast.getStrSProgram ast )
   putStrLn "-----------------AST'--------------------"
   putStr ( AstPrime.getStrAstPrime ast' )
   putStrLn "-----------------Eval--------------------"
   putStr ( AstPrime.getStrAstPrime result )
   putStrLn "-----------------------------------------"

batch :: [String] -> IO ()
batch ( file : files) = do
   lib <- readFile "splCore.spl"
   buf <- readFile file
   putStrLn ("Compiling: " ++ file)
   process file (lib ++ buf)
   batch files
batch [] = return ()

singleLine :: String -> String -> IO ()
singleLine lib input = do
   process "" (lib ++ input)
   interactive lib

interactive :: String -> IO ()
interactive lib = do
   putChar '>'
   hFlush stdout
   input <- getLine
   if input == ":q"
      then return ()
      else singleLine lib input

interactiveInit :: IO ()
interactiveInit = do
   lib <- readFile "splCore.spl"
   interactive lib

-- repeat the processing for each argument
main :: IO ()
main = 
   do args <- getArgs
      if badUsage args 
         then putStrLn "usage syntax: ./splc <input>.spl+"
         else if length args > 0
                  then batch args
                  else interactiveInit
