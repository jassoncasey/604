import System( getArgs )
import Control.Monad( forM )

import CmdOpts
import Interactive
import ParseTree
import Parser
import qualified Ast 

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
printIfValid (fn,src) = fn ++ "\n" ++ (foldr (++) "" (getStmts fn src)) ++ "\n\n"

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

--printEvaluatable :: Expression -> Bool
--printEvaluatable ErrExpr = ""
--printEvaluatable _

main :: IO ()
main = do
   args <- getArgs
   let ( mode, errorMessage ) = verifyArgs args
   case mode of
      Interactive -> startInteractive
      Batch -> batch args
      Error -> putStrLn errorMessage
