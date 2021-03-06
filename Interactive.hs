module Interactive
( startInteractive
) where

import System.IO
--import Data.List( isPrefixOf )

import Eval
import Lexer
import PrettyPrint
import Environment
import Parser
--import ParseTree
import qualified Ast

-- Fires up the interactive interpreter
startInteractive :: IO ()
startInteractive = do
  putStr promptIntro
  coreSource <- readFile "splCore.spl"
  --error coreSource
  let (env,_) = evalProgramInter (Env []) $ Ast.transformProg $ parse "" coreSource
  replSpl env

-- Constant Strings --
promptStr :: String
promptStr = "> "
promptIntro :: String
promptIntro = "spli, the spl interpreter. For options, enter ':help'.\n"
promptHelp :: String
promptHelp = "Command options-\n"
  ++ "  :quit         Exit the interpreter.\n"
  ++ "  :help         Display command information.\n\n"
  ++ "  :lex s        Displays token information from lexed string s.\n"
  ++ "  :parse s      If parse is successful, 'valid' is displayed."
  ++ "Otherwise display\n"
  ++ "                the error.\n"
  -- ++ "  :environment  Displays current environment information\n"


-- Puts str in the output buffer and immediately flushes it to stdout handle
-- meaning, print str to screen immediately, avoids buffer overwrite
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Print prompt and get input
readPrompt :: IO String
readPrompt = flushStr promptStr >> getLine

isCmd :: String -> Bool
isCmd "" = False
isCmd s = ':' == (head $ dropWhile (==' ') s)

breakCmd :: String -> (String,String)
breakCmd cmdStr =
  let trimmedCommand = dropWhile (==' ') $tail $ dropWhile (==' ') cmdStr
  in
    break (==' ') trimmedCommand

-- FIXME Allow it to print errors
printTokens :: String -> String
printTokens s = (printTokenList $ tokenizeBuff "-" s)

-- FIXME Allow it to print errors
printParseTree :: String -> String
printParseTree s = 
  case k of
    Ast.ErrProg -> "Failed to pase expression."
    Ast.Prog exprs -> foldr (++) "" $ map Ast.getStrSExpression exprs
  where k = Ast.transformProg $ parse "-" (s ++ ";")

-- Semi-colon is not required to appear at the end of an expression when in
-- interactive mode
addSemiIfNot :: String -> String
addSemiIfNot "" = ";"
addSemiIfNot s
 | last s == ';' = s ++ ";"
 | otherwise     = s

tryEval :: Env -> String -> (Env, String)
tryEval env "" = (env,"")
tryEval env s =
  let
    (env', e) = evalProgramInter env $
      Ast.transformProg $ parse "-" s
  in
    (env', (Ast.getStrExpression e) ++ "\n")


  {-ErrPrg -> (env, "Malformed expression. FIXME Better error?\n")
  parseProg -> case Ast.transformProg parseProg of
    Ast.ErrProg -> (env, "Enable to transform parse tree to AST.\n")
    a -> (newState, Ast.getStrExpression)
    where (newState, expr) = evalProgramInter env a-}

replSpl :: Env -> IO ()
replSpl state = do
  input <- readPrompt
  if isCmd input
    then do
      let (cmd, rest) = breakCmd input
      case cmd of
        "quit"  -> putStrLn "Leaving SPL interactive."
        "help"  -> putStr promptHelp >> replSpl state
        "lex"   -> putStr (printTokens rest) >> replSpl state
        "parse" -> putStr ((printParseTree rest) ++ "\n") >> replSpl state
        _       -> putStrLn "Unknown command." >> replSpl state
    else do
      let (newState, expr) = tryEval state input
      putStr expr >> replSpl newState

-- Old part of REPL
      {-case (parse "-" $ addSemiIfNot input) of
        ErrPrg _ -> putStrLn "Malformed expression" >> replSpl state
        rawProg -> case Ast.transformProg rawProg of
          Ast.ErrProg -> putStrLn "Uncaught error on parse tree to AST tranformation" >> replSpl state
          prgm -> do
            let (newState, expr) = evalProgramInter state prgm
            putStrLn (Ast.getStrExpression expr) >> replSpl newState-}
