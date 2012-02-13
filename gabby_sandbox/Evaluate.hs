module Evaluate( eval ) where

import Ast



eval :: Ast -> Ast

-- Evaluate arithmetic
eval (Application (Application (Constant (Primitive op val)) ast) ast') =
  case (eval_ast,eval_ast') of
    (Constant (IntCst a),Constant (IntCst b)) | elem op ["+","-","*","/"]
      -> deltaRule op a b
    (_,_) -> (Application (Application (Constant (Primitive op val)) eval_ast) eval_ast')
  where eval_ast = eval ast
        eval_ast' = eval ast'

eval ast = ast

deltaRule :: String -> Integer -> Integer -> Ast
deltaRule "+" a b = Constant (IntCst (a + b))
deltaRule "-" a b = Constant (IntCst (a + b))
deltaRule "*" a b = Constant (IntCst (a + b))
deltaRule "/" a b = Constant (IntCst (quot a b))
deltaRule op _ _ = error ("Passed unrecognized operation: " ++ op ++ "\n")