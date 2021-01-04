module Grammar where

import Array

data Type 
    --
    = IntegerType Int
    -- 
    | BooleanType Bool
    --
    | ArrayType (Array Int)
    deriving Show

data AExp
    -- Constant integer
    = AExpConstant Int
    -- Identifier string
    | AExpVariable String
    -- value contained in a specific position of an array variable
    | ValueFromArray String AExp
    -- Addition between sub-expressions
    | Add AExp AExp
    -- Subtraction between sub-expressions
    | Sub AExp AExp
    -- Multiplication between sub-expressions
    | Mul AExp AExp
    deriving Show

data BExp
    -- Ground True and False
    = BExpConstant Bool
    -- Identifier string
    | BExpVariable String
    -- Not unary operator
    | Not BExp
    -- Or binary operator
    | Or BExp BExp
    -- And binary operator
    | And BExp BExp
    -- < binary operator between arithmetical expressions
    | Less AExp AExp
    -- > binary operator between arithmetical expressions
    | Greater AExp AExp
    -- >= binary operator between arithmetical expressions
    | LessEqual AExp AExp
    -- <= binary operator between arithmetical expressions
    | GreaterEqual AExp AExp
    -- == binary operator between arithmetical expressions
    | Equal AExp AExp
    -- != binary operator between arithmetical expressions
    | NotEqual AExp AExp
    deriving Show

data ArrayExp
    --  x = [1,2,3]
    = ArrayValues [AExp]
    -- x = y       y = [1,2,3]
    | ArrayExpVariable String
    deriving Show

-- Command declaration
data Command
    --
    = AExpDeclaration String AExp
    --
    | BExpDeclaration String BExp
    --
    | ArrayDeclaration String AExp
    --
    | Skip
    --
    | AExpAssignment String AExp
    --
    | BExpAssignment String BExp
    --
    | ArrayAssignmentSingleValue String AExp AExp
    --
    | ArrayAssignmentValues String ArrayExp
    --
    | IfThenElse BExp [Command] [Command]
    --
    | While BExp [Command]
    deriving Show

-- Program declaration
type Program = [Command]
