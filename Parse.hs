import Data.Either


{- Parser-
    The parse is constructed out of an initializer, rules and a terminator. The
  Approach is based on the recursive descent compiler design. An initializing
  function initParse starts parsing a tokenized statement (type [Token]). From
  the init function rules are called. A rule is a binary function parseRule that
  constructs more of the parse tree from the arguments its given.
-}




data Expr = LitExpr Token | IdExpr Token | BinOpExpr Token Expr Expr
-- Allow graceful error reporting, mimics Data.Either, but warrents own implementation
data Expr' = Expr | ExprNull | ExprError String
newtype Statement = Expr | NullExpr | InvalidExpr String
newtype Program = [Statement]


-- Binary operator strength, assume only plus for now?
--parseStatement :: [Token] -> Expr
--parseStatement [] = --ExprError "Empty Statement."    -- Should this fault?


-- Starst parsing
-- Valid start tokens are TokSemi, TokLambda, TokLet, TokLit, TokId and TokParL
initParse [Token] -> Expr'
initParse [] = ExprNull
initParse [Token TokSemi] = ExprNull
initParse [Token _ _ _ l _] = ExprError "Non-terminating line " ++ (show l) ++ "."
initParse (t:t':tkns) = []

parseRule :: Token -> Token -> 


parseExpr :: [Token] -> Expr
