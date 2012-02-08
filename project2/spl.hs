import System( getArgs )
import Control.Monad( forM )

import CmdOpts
import Interactive
import ParseTree
import Parser
import qualified Ast 
import Eval
import qualified Environment

-- Process files for project 2
batch :: [String] -> IO ()
batch args = do
  sources <- forM args readFile
  let print = map printIfValid $ zip args sources
  mapM putStr $ print
  return ()

-- Print the AST's and results of AST's if valid
-- Input (filename, source)
printIfValid :: (String,String) -> String
printIfValid (fn,src) =
  let
    Ast.Prog k = Ast.transformProg $ parse fn src
    (_,_,s) = evalExprsString (Environment.Env []) k
  in
    fn ++ "\n" ++ s ++ "\n\n"

--printStmtAndFilenames :: S

-- Takes fn and source, prints source, statement by statement
getStmts :: String -> String -> [String]
getStmts fn src =
  case pSrc of
    Prog sts   -> map printStmt sts
    ErrPrg sts -> map printStmt sts
  where pSrc = parse fn src

printStmt :: Statement -> String
printStmt EmptyStmt = ""
printStmt (ErrStmt err) = err
printStmt stmt = (Ast.getStrSExpression $ Ast.transformStmt stmt) ++ "\n"

-- get eval results and parsing
--getParseEval :: Ast.Expression -> String

parseAndTransform :: String -> String -> Ast.Program
parseAndTransform fn src = Ast.transformProg $ parse fn src

--astToEvalPair :: Ast.Expression -> (Ast.Expression,Ast.Expression)
--astToEvalPair e = (Eval.evalExpr


main :: IO ()
main = do
   args <- getArgs
   let ( mode, errorMessage ) = verifyArgs args
   case mode of
      Interactive -> startInteractive
      Batch -> batch args
      Error -> putStrLn errorMessage
