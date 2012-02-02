module ParseTree (
   Expression(..),
   Statement(..),
   Program(..)
) where

-- Import the lexer and make 
-- token use less verbose
import Lexer as Lexer

{- Here is the LL grammar for SPL, the parse representation
   needs to be in exact correspondance with the the grammar
 
   1. Terminal keywords are captialized
   2. Non-terminals keywords are lowercase
   3. Keyword, operator, and partial non-terminals are 
      represented with single quote literals
   4. Simple Programming Language (SPL) parse representation
   5. We want to keep around the tokens for error reporting 
   6. During type checking and evaluation stage
-}
 
{-
 - program  : Empty
            | statements
 
   statements  : statement
               | statement statements
-}

data Program = 
   Prog [Statement]
  | ErrPrg [Statement]
  deriving (Show,Eq)

{- 
   statement : expression ';'
-}

data Statement =
   Stmt Expression Token
   | EmptyStmt
   | ErrStmt String
   deriving (Show,Eq)

{- 
   expression  : 'let' Id '=' expression
               | '\' Id '.' expression
               | term
 
   term  : factor '+' term
         | factor '-' term
         | factor
 
   factor   : application '*' factor
            | application '/' factor
            | application
 
   application : primary application
               | primary
 
   primary  : Id
            | Digit
            | '(' compound ')'

   complex  : expression
            | expression ';' compound
 -}

data Expression = 
   Id Token
   | Nat Token
   | Let Token Expression Token Expression
   | Lambda Token Expression Token Expression
   | Unary Token Expression
   | Binary Token Expression Expression
   | Application Expression Expression
   | Compound Token [(Expression, Token)] Token
   | ErrExpr String
   deriving (Show,Eq)
