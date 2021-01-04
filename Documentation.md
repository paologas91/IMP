# Index
- [1. Introduction](#1-introduction)
- [2. IMP Grammar](#2-imp-Grammar)
- [3. Environment](#3-environment)
- [4. Internal Representation](#4-internal-representation)
  - [4.1. Arrays](#4.1-arrays)
- [5. Design](#5-design)
  - [5.1. Parser Implementation](#5.1-parser-implementation)
    - [5.1.1. Functor, Applicative, Monad and Alternative](#5.1.1-functor,-applicative,-monad-and-alternative)
    - [5.1.2. Arithmetic Expression Parsing](#5.1.2-arithmetic-expression-parsing)
    - [5.1.3. Boolean Expression Parsing](#5.1.3-boolean-expression-parsing)
    - [5.1.4. Command Parser](#5.1.4-command-parser)
    - [5.1.5. Program Parser](#5.1.5-program-parser)
  - [5.2. Interpreter Implementation](#5.2-interpreter-implementation)
    - [5.2.1. Arithmetic Expressions Evaluation](#5.2.1-arithmetic-expressions-evaluation)
    - [5.2.2. Boolean Expressions Evaluation](#5.2.2-boolean-expressions-evaluation)
    - [5.2.3. Array Expressions Evaluation](#5.2.3-array-expressions-evaluation)
    - [5.2.4. Commands Evaluation](#5.2.4-commands-evaluation)
- [6. Running an example](#6-running-an-example)

# 1. Introduction
This is the documentation of a simple interpreter for the IMP language written using Haskell programming language.
An interpreter, by definition, is a computer program that directly executes instructions written in a programming or scripting language without requiring them previously to have been compiled into a machine language program. The interpreter, indeed, usually transforms the high-level program into an intermediate language before executing it.

IMP is a simple imperative language which gives us the possibilities to use the basic constructs used in the most 
common imperative languages. These are:
1.  <strong>assignment</strong>: assigns a value to a variable;
2.  <strong>skip</strong>: just does nothing;
3.  <strong>if then else</strong>: if the boolean expression after the <em>if</em> it's true, <em>then</em>  executes some commands otherwise (<em>else</em>) executes some other ones;
4.  <strong>while</strong>: executes and continue executing the commands while the boolean condition remains true.

The IMP interpreter uses an <strong>eager evaluation strategy</strong>. This means that, in case of composition between functions, the inner one is executed first and so on until the most external is reached. In order to perform this, the interpreter uses a <strong>call-by-value</strong>.

# 2. IMP Grammar
The grammar is shown below.

```haskell
program ::= <command> | <command> <program>

command ::= <variableDeclaration>  
          | <skip>
          | <assignment> 
          | <ifThenElse> 
          | <while>

variableDeclaration ::= <AExpDeclaration>
                      | <BExpDeclaration>
                      | <ArrayDeclaration>

assignment ::= <AExpAssignment>
             | <BExpAssignment>
             | <ArrayAssignmentSingleValue>
             | <ArrayAssignmentValues>
                      
AExpDeclaration ::= "int" <identifier> "=" <aexp> ";"

BExpDeclaration ::= "bool" <identifier> "<-" <bexp> ";"

ArrayDeclaration ::= "array" <identifier> "[" <aexp> "]" ";"

AExpAssignment ::= <identifier> "=" <aexp> ";" 

BExpAssignment ::= <identifier> "<-" <bexp> ";"

ArrayAssignmentSingleValue ::= <identifier> "[" <aexp> "]" ":=" <aexp> ";"

ArrayAssignmentValues ::= <identifier> ":=" "[" <aexp> ["," <aexp>]* "]" ";"
                        | <identifier> ":=" <identifier> ";"

ifThenElse ::= "if" "(" <bexp> ")" "{" <program> "}"
             | "if" "(" <bexp> ")" "{" <program> "}" "else" <program> "}"

while ::= "while" "(" <bexp> ")" "{" <program> "}"

skip ::= "skip" ";"

aexp ::= <aterm> ["+" <aterm>]*
       | <aterm> ["-" <aterm>]*

aterm ::= <afact> ["*" <afact>]*

afact ::= <integerNumber> 
        | <identifier> 
        | "(" <aexp> ")" 
        | <identifier> "[" <aexp> "]"

bexp ::= <bterm> ["or" <bterm>]*

bterm ::= <bfact> ["and" <bfact>]*

bfact ::= "True" 
        | "False" 
        | "not" <bexp>
        | "(" <bexp> ")"   
        | <aexp> "<" <aexp>
        | <aexp> "<=" <aexp>
        | <aexp> ">" <aexp>
        | <aexp> ">=" <aexp>
        | <aexp> "==" <aexp>
        | <aexp> "!=" <aexp>
        | <identifier>  

integerNumber ::= "-" <naturalNumber>
                | <naturalNumber>

naturalNumber ::= <digit> 
                | <digit> <naturalNumber>

digit ::= 0 | 1 | 2 | 3 | 5 | 6 | 7 | 8 | 9

identifier ::= <letter> 
             | <letter> <digit>

letter ::= <lowercase> 
         | <uppercase>

lowercase ::= "a-z"

uppercase ::= "A-Z"

```

# 3. Environment
The interpreter consists of two parts: the first part takes one input file as a String and creates an internal representation of the program (the parser) and the second one which takes the output of the first part and evaluates the program updating the state of the memory (the interpreter). 

To keep track of the variables and their values during the program execution, it has been created a Dictionary data structure.
```haskell
type State = Dictionary String Type
```
In this way I can represent the state of the memory by using a dictionary where every variable is kept stored with its corresponding value. In this case we are able to store values of type Integer, Boolean or Array of Integers.
These three kind of types are grouped into an Haskell data type and defined as Type.
The type Array has been defined in a separated module and will be discussed in the next topic.
```haskell
data Type = IntegerType Int | BooleanType Bool | ArrayType (Array Int)
```
Another architectural choice consists in the fact that we are able only to store Integer values into the arrays.

The dictionary data structure is implemented as it follows:
```haskell
newtype Dictionary k v = Dictionary [(k, v)]

get :: (Eq k) => Dictionary k v -> k -> Maybe v
get (Dictionary []) _ = Nothing
get (Dictionary ((h, v) : ps)) k =
  if k == h
    then Just v
    else get (Dictionary ps) k

insert :: (Eq k) => Dictionary k v -> k -> v -> Dictionary k v
insert (Dictionary []) k v = Dictionary [(k, v)]
insert (Dictionary ((h, u) : ps)) k v =
  if k == h
    then Dictionary ((h, v) : ps)
    else Dictionary ((h, u) : ds)
  where
    (Dictionary ds) = insert (Dictionary ps) k v
```
In the project the types of k and v are respectively String and Type:
- String indicates instead the name of the variable;
- Type stays for the value contained into the variable (Integer, Boolean or Array as described previously)

Basically, the dictionary (which represents the environment) can be seen as a memory and therefore must be kept up to date: the instructions/commands that modify the state of the memory are the assignments.

If, for example, at the end of a program execution, the environment contains only two variables, one named x which contains the value 8 and another one named y containing the value 1. In this case the representation will be:
```haskell
[("x", 8), ("y", 1)]
```

# 4. Internal representation
As said before, the parser takes a string as input and creates an internal representation of the program. This representation has been defined as something really similar to the grammar of IMP.

A program is defined as a list of commands:
```haskell
type Program = [Command]
```
A command can be:
- a declaration of a variable which will contain the result of an arithmetic expression. In this step it's mandatory to assign a value to the variable;
- a declaration of a variable which will contain the result of a boolean expression. In this step it's mandatory to assign a value to the variable;
- a declaration of a variable which will contain an array. I also have to specify the size of the array;
- skip, which simply does nothing;
- an assignment of an arithmetic expression (more properly we assign its result) to a variable previously declared (which has been declared in the proper way, that is, to store arithmetic expressions);
- an assignment of a boolean expression (more properly we assign its result) to a variable previously declared (which has been declared in the proper way, that is, to store boolean expressions);
- an assignment of an arithmetic expression (more properly we assign its result) to a specific position of the array variable previously declared;
- an assignment to an array variable of:
  - a list of arithmetic expressions (E.g. [1, 2, 3] or [1, 2*3, 4-1]);
  - an array variable previously declared.
- if-then-else construct, which evaluates the boolean expression and if it's true then executes the sub-program after the <em>then</em> keyword, otherwise exectues the one after the <em>else</em> keyword;
- while contruct, which evaluates the boolean condition and executes the sub-program in the parenthesis until it remains true.
```haskell
data Command
        = AExpDeclaration String AExp
        | BExpDeclaration String BExp
        | ArrayDeclaration String AExp
        | Skip
        | AExpAssignment String AExp
        | BExpAssignment String AExp
        | ArrayAssignment String AExp AExp
        | ArrayAssignmentValues String ArrayExp
        | IfThenElse BExp [Command] [Command]
        | While BExp [Command]
```
An arithmetic expression can be:
- a constant;
- a variable which is able to store an arithmetic expression;
- a value contained in a specific position of an array variable;
- an operation between two arithmetic expressions (in this case Add, Sub and Mul that indicate respectively the sum, the subtraction and the multiplication)
```haskell
data AExp
    = AExpConstant Int
    | AExpVariable String
    | ValueFromArray String AExp
    | Add AExp AExp
    | Sub AExp AExp
    | Mul AExp AExp
```
The internal representations deals with the boolean expressions which can be:
- a boolean value;
- a variable which is able to store a boolean expression;
- an operator between one or two boolean expressions (in this case Not, which is defined on one boolean expression only, Or and And which are defined on two boolean expressions);
- an operator between two arithmetic expressions (less or equal, less, greater or equal, greater, equal or not equal)
```haskell
data BExp
    = BExpConstant Bool
    | BExpVariable String
    | Not BExp
    | Or BExp BExp
    | And BExp BExp
    | Less AExp AExp
    | Greater AExp AExp
    | LessEqual AExp AExp
    | GreaterEqual AExp AExp
    | Equal AExp AExp
    | NotEqual AExp AExp
```
The last internal representation regards the arrays which can be:
- a list of arithmetic expressions (E.g. [1, 2, 3] or [1, 2*3, 4-1]);
- an array variable already declared previously.
```haskell
data ArrayExp
    = ArrayValues [AExp]
    | ArrayExpVariable String
```

# 4.1. Arrays
The arrays are defined as follow in the module <code>Array.hs</code>:
```haskell
type Array a = [a]
```
with the functions:
- declare an array of fixed size with all zeroes;
- read a value from a given position of the array;
- write a value in the given position of the array.
```haskell
declare :: Int -> Array Int
declare n = replicate n 0

read :: Int -> Array a -> Maybe a
read _ [] = error "Out of bound"
read i (v : vs)
  | i == 0 = Just v
  | i < 0 = error "Out of bound"
  | otherwise = Array.read (i - 1) vs

write :: Int -> a -> Array a -> Maybe (Array a)
write _ _ [] = error "Out of bound"
write i v' (v : vs)
  | i == 0 = Just (v' : vs)
  | i < 0 = error "Out of bound"
  | otherwise = case write (i - 1) v' vs of
                    Just vs' -> Just (v : vs')
                    Nothing -> error "Out of bound"
```
# 5. Design
The interpreter consists of two parts: the first part takes one input file as a String and creates an internal representation of the program (the parser) and the second one which takes the output of the first part and evaluates the program updating the state of the memory (the interpreter).

We start from the analysis of the parser.
# 5.1. Parser implementation
A parser can be seen as a function which takes a string as input and produces a couple as output:
```haskell
newtype Parser a = P (String -> Maybe (a, String))
```
where <em>a</em> is the parametrized type and indicates the type of the parser; the string returned with <em>a</em> represents instead the part of the input which has still not be parsed or that failed the parsing procedure. This means that if at the end, the empty string is returned, the parsing has been completed successfully.

# 5.1.1. Functor, Applicative, Monad and Alternative
In order to combine parsers together we need to create instances of Functor, Applicative, Monad and Alternative.

The functor allows to apply a function <em>g</em> to a value wrapped in a parser <em>p</em>. 
This way I define the <code>fmap</code> function for parser which takes as input a function and an object wrapped inside a functor. The result is then the application of the function to the unwrapped object inside the functor.
```haskell
instance Functor Parser where
  fmap g (P p) = P (\input -> case p input of
    Nothing -> Nothing
    Just (v, out) -> Just (g v, out))
```
The applicative, which can be used only if <em>Functor</em> has been already defined, let us to apply a function wrapped in a parser <em>pg</em> to another parser <em>px</em>. It defines the <code>pure</code> and the <code><*></code> operators. The first one just takes as input an object and wraps it into an applicative. The second one instead, takes as input a function and an object both wrapped inside the applicative. The result is the application of the unwrapped function to the unwrapped object.
```haskell
instance Applicative Parser where
  pure v = P (\input -> Just (v, input))
  (P pg) <*> px = P (\input -> case pg input of
    Nothing -> Nothing
    Just (g, out) -> p out where (P p) = fmap g px)
```
The monad, which can be used only if the <em>Applicative</em> has been already defined, is used to apply a function which returns a wrapped parser to a wrapped parser. It defines the <code>return</code> and the <code>bind</code> operators. The first one returns an object wrapped inside a Monad. The second one is denoted with <code>>>==</code> and takes as input an object wrapped inside a Monad and a function. The result of the <em>bind</em> operator is the application of the function to the unwrapped object. Since <code>Parser</code> is a monadic type, the do notation can be used to have a sequence of parsers and process their resulting values.
```haskell
instance Monad Parser where
  (P p) >>= f = P (\input -> case p input of
                         Nothing -> Nothing
                         Just (v, out) -> q out where (P q) = f v)
```
In order to explore all the possible ways when trying to match for an expression of the grammar we use the <code><|></code> operator. By using it, supposing we have two parsers like P and Q, if the first one fails then we will have only the output of the second parser, otherwise just the output of the first. By using this we can use some useful functions like <em>some</em> and <em>many</em>.
```haskell
instance Alternative Parser where
  empty = P (\input -> Nothing)
  (P p) <|> (P q) = P (\input -> case p input of
                                  Nothing -> q input
                                  Just (v, out) -> Just (v, out))
```
With <em>Alernative</em> we are able to define and use some combinators like <em>some</em>, <em>many</em> and <em>chain</em> in the <code>do</code> construct which can be used thanks to the implementation of the monads.

- The <em>some</em> construct let us to use many parsers together. In this way at least the first parser has to succed;
- The <em>many</em> it's like the previous one with the difference that every parser could fail;
- The <em>chain</em> implements the left-associative order in the evaluation of arithmetic and boolean expressions.
```haskell
class Monad m => Alternative m where
  empty :: m a
  (<|>) :: m a -> m a -> m a
  many :: m a -> m [a]
  many x = some x <|> return []
  some :: m a -> m [a]
  some x = (:) <$> x <*> many x
  chain :: m a -> m (a -> a -> a) -> m a
  chain p op = do a <- p; rest a 
   where rest a = (do f <- op; a' <- p; rest (f a a')) <|> return a
```
# 5.1.2. Arithmetic Expression Parsing
This parser is built in such a way to ensure that for each arithmetic expression will be a single derivation tree. The parser function <strong>aExpParser</strong> is supported by <strong>aTermParser</strong> and <strong>aFactorParser</strong>. These parsers basically are needed to create an order of execution of the arithmetic expressions: given, for example, more priority to multiplication rather than addition and subtraction, and also to execute first the expressions in the brackets.

We can also see the use of the <code>chain</code> for the sum and the subtraction. 
```haskell
aExpParser :: Parser AExp
aExpParser = do chain aTermParser op
   where op = (do keywordParser "+"; return Add)
               <|> do keywordParser "-"; return Sub

aTermParser :: Parser AExp
aTermParser = do chain aFactParser op
               where op = do keywordParser "*"; return Mul

aFactParser :: Parser AExp
aFactParser = (do a <- integerNumberParser; return (AExpConstant a))
                <|> do 
                   a <- identifierParser
                   do 
                      keywordParser "["
                      n <- aExpParser
                      keywordParser "]"
                      return (ValueFromArray a n)
                      <|>
                      return (AExpVariable a)
                <|> do 
                     keywordParser "("
                     a <- aExpParser
                     keywordParser ")"
                     return a
```
# 5.1.3. Boolean Expression Parsing
A parser for evaluate boolean expressions has been created and the functioning is similar to the one of the arithmetic case: for each boolean expression exists a unique derivation tree. In this case, the and operator has an higher priority with respect to or operation. Moreover, <strong>bExpParser</strong> could need to use <strong>aExpParser</strong>, for example when a comparison between two numbers is required.
```haskell
bExpParser :: Parser BExp
bExpParser = do chain bTermParser op 
               where op = do keywordParser "or"
                             return Or
       
bTermParser :: Parser BExp
bTermParser = do chain bFactParser op
               where op = do keywordParser "and"
                             return And

bFactParser :: Parser BExp
bFactParser = do keywordParser "True"
                 return (BExpConstant True)
              <|>
              do keywordParser "False"
                 return (BExpConstant False)
              <|>
              do keywordParser "not"
                 a <- bExpParser
                 return (Not a)
              <|>
              do keywordParser "("
                 b <- bExpParser
                 keywordParser ")"
                 return b
              <|>
              do {
                 a1 <- aExpParser;
                 do keywordParser "<"
                    a2 <- aExpParser
                    return (Less a1 a2)
                  <|>
                  do keywordParser "<="
                     a2 <- aExpParser
                     return (LessEqual a1 a2)
                  <|>
                  do keywordParser ">"
                     a2 <- aExpParser
                     return (Greater a1 a2)
                  <|>
                  do keywordParser ">="
                     a2 <- aExpParser
                     return (GreaterEqual a1 a2)
                     <|>
                  do keywordParser "=="
                     a2 <- aExpParser
                     return (Equal a1 a2)
                     <|>
                  do keywordParser "!="
                     a2 <- aExpParser
                     return (NotEqual a1 a2)
              }
              <|>
              do a <- identifierParser
                 return (BExpVariable a)
```
# 5.1.4. Command Parser
The parser for commands is defined as:
```haskell
commandParser :: Parser Command
commandParser = variableDeclParser
                <|> skipParser
                <|> assignmentParser
                <|> ifThenElseParser
                <|> whileParser
```
where <code>variableDeclParser</code> can be of 3 different types:
- The first one let us to declare a variable for arithmetic expressions with the keyword <code>int</code> (Example: <code>int a;</code>);
- The second one let us to declare a variable for boolean expressions with the keyword <code>bool</code> (E.g.: <code>bool a;</code>);
- The third one let us to declare an array of integers with the keyword <code>array</code> (E.g.: <code>array a[10];</code> is an array of integers of size 10).

As decribed previously, it's also mandatory to assign a value for the first two cases.
The implementation is the following:
```haskell
variableDeclParser :: Parser Command
variableDeclParser = do {
   keywordParser "int";
   i <- identifierParser;
   keywordParser ":=";
   a <- aExpParser;
   keywordParser ";";
   return (AExpDeclaration i a)
   } <|> do {
   keywordParser "bool";
   i <- identifierParser;
   keywordParser ":=";
   b <- bExpParser;
   keywordParser ";";
   return (BExpDeclaration i b)
   } <|> do {
   keywordParser "array";
   i <- identifierParser;
   keywordParser "[";
   n <- aExpParser;
   keywordParser "]";
   keywordParser ";";
   return (ArrayDeclaration i n)
   }
```
The assignment parser is the same like the declaration one with the difference that we are not declaring a new variable but we are using an existing one to assign it a new value. For array we can assign a value for every valid position of the array but we cannot assign another whole array.
```haskell
assignmentParser :: Parser Command
assignmentParser = do i <- identifierParser
                      do keywordParser ":="
                         a <- aExpParser
                         keywordParser ";"
                         return (AExpAssignment i a)
                       <|>
                       do keywordParser ":="
                          b <- bExpParser
                          keywordParser ";"
                          return (BExpAssignment i b)
                       <|> 
                       do keywordParser "["
                          i' <- aExpParser
                          keywordParser "]"
                          keywordParser ":="
                          a <- aExpParser
                          keywordParser ";"
                          return (ArrayAssignmentSingleValue i i' a)
                       <|>
                       do keywordParser ":="
                          keywordParser "["
                          i' <- aExpParser
                          i'' <- many (do keywordParser ","; aExpParser)
                          keywordParser "]"
                          keywordParser ";"
                          return (ArrayAssignmentValues i (ArrayValues (i':i'')))
                       <|>
                       do keywordParser ":="
                          x <- identifierParser
                          keywordParser ";"
                          return (ArrayAssignmentValues i (ArrayExpVariable x))
```
The following is the <em>skip</em> parser:
```haskell
skipParser :: Parser Command
skipParser = do keywordParser "skip"
                keywordParser ";"
                return Skip
```
Here we have the <em>if-then-else</em> parser:
```haskell
ifThenElseParser :: Parser Command
ifThenElseParser = do keywordParser "if"
                      keywordParser "("
                      b <- bExpParser
                      keywordParser ")"
                      keywordParser "{"
                      thenCase <- programParser
                      do keywordParser "}"
                         keywordParser "else"
                         keywordParser "{"
                         elseCase <- programParser
                         keywordParser "}"
                         return (IfThenElse b thenCase elseCase)
                       <|>
                       do keywordParser "}"
                          return (IfThenElse b thenCase [Skip])s
```
Here the <em>while</em> parser:
```haskell
whileParser :: Parser Command
whileParser = do keywordParser "while"
                 keywordParser "("
                 b <- bExpParser
                 keywordParser ")"
                 keywordParser "{"
                 p <- programParser
                 keywordParser "}"
                 return (While b p)
```
# 5.1.5. Program Parser
The <em>program</em> parser is defined as many parsers for commands as the following code suggests:
```haskell
programParser :: Parser [Command]
programParser = do many commandParser
```
In this way, I can have a program which is empty or a program with a potentially infinite number of commands.

To execute the parser the <code>exeParser</code> function has been defined:
```haskell
exeParser :: String -> ([Command], String)
exeParser s = case result of
                Nothing -> error "Parsed not executed"
                Just (cs, str) -> (cs, str)
              where (P p) = programParser
                    result = p s
```
so that the result of the parser is a list of commands and a string. If the parsing fails, in the sense that there are some errors in the execution of the parser module, then an error is raised; if instead the parsing fails, in the sense that the structure of the program given as input doesn't fit the grammar of IMP, the list of commands contains the part of the program successfully parsed and, the string, the rest of the program which has not been parsed; if lastly it succeds, then the string will be empty and the list of commands will contain all the commands to give as input to the interpreter which is going to evaluate the final result (if it there exists).

# 5.2. Interpreter Implementation
The main function of the interpreter is to take the output of the parser and work on it. When indeed the parser computes correctly the program, as we said before it returns and internal representation of it and the interpreter has to compute the final result.

For example we have a program to execute defined as follow:
```
int i = 1;
int n = 5;
int x = 1;
while (i <= n) {
  x = x * i;
  i = i + 1;
}
```
then the internal representation of the program which is given as output from the parser and given as input to the interpreter is:
```
[ AExpDeclaration "i" (AExpConstant 1), 
  AExpDeclaration "n" (AExpConstant 5),
  AExpDeclaration "x" (AExpConstant x),
  while (Less (AExpVariable "i") (AExpVariable "n"))
    [ AExpAssignment "x" (Mul (AExpVariable "x") (AExpVariable "i")),
      AExpAssignment "i" (Add (AExpVariable "i") (AExpConstant 1))
    ]
]
```
The interpreter will then compute the program and give us the final result (in this case 120).

At the end, the state of the memory will so be the following:
```haskell
[("i",IntegerType 6),("n",IntegerType 5),("x",IntegerType 120)]
```

To make this possible we will need to evaluate arithmetic expressions, boolean expressions and commands.

The results of every single evaluation is a <em>Maybe</em> type so that we can distinguish easily a failure (and raise an error with a custom message to help the user in understanding) from a correct execution.

# 5.2.1. Arithmetic Expressions Evaluation
The evaluation of an arithmetic expression takes a state as input (which represents an internal representation of the memory) and an arithmetic expression (coded in the internal reprentation way) and gives <code>Just Int</code> or <code>Nothing</code> depending of the success or failure of the computation.
```haskell
aExpEval :: State -> AExp -> Maybe Int
aExpEval _ (AExpConstant c) = Just c
aExpEval s (AExpVariable v) = 
  case get s v of
    Just (IntegerType r) -> Just r 
    Just (BooleanType _) -> error "Variable of type boolean!"
    Just (ArrayType _) -> error "Variable of type array!"
    Nothing -> error "Variable not found!"
aExpEval s (ValueFromArray v i) =
  case get s v of
    Just (IntegerType _) -> error "Variable of type integer!"
    Just (BooleanType _) -> error "Variable of type boolean!"
    Just (ArrayType r) -> Array.read i' r
                            where Just i' = aExpEval s i
    Nothing -> error "Variable not found!"
aExpEval s (Add a b) = (+) <$> aExpEval s a <*> aExpEval s b
aExpEval s (Sub a b) = (-) <$> aExpEval s a <*> aExpEval s b
aExpEval s (Mul a b) = (*) <$> aExpEval s a <*> aExpEval s b
```
# 5.2.2. Boolean Expressions Evaluation
The evaluation of a boolean expression takes a state as input (which represents an internal representation of the memory) and a boolean expression (coded in the internal reprentation way) and gives <code>Just Bool</code> or <code>Nothing</code> depending of the success or failure of the computation.
```haskell
bExpEval :: State -> BExp -> Maybe Bool
bExpEval _ (BExpConstant b) = Just b
bExpEval s (BExpVariable v) = 
  case get s v of
    Just (IntegerType _) -> error "Variable of type integer!"
    Just (BooleanType r) -> Just r
    Just (ArrayType _) -> error "Variable of type array!"
    Nothing -> error "Variable not found"
bExpEval s (Not b) = not <$> bExpEval s b
bExpEval s (Or a b) = (||) <$> bExpEval s a <*> bExpEval s b
bExpEval s (And a b) = (&&) <$> bExpEval s a <*> bExpEval s b
bExpEval s (Less a b) = (<) <$> aExpEval s a <*> aExpEval s b
bExpEval s (LessEqual a b) = (<=) <$> aExpEval s a <*> aExpEval s b
bExpEval s (Greater a b) = (>) <$> aExpEval s a <*> aExpEval s b
bExpEval s (GreaterEqual a b) = (>=) <$> aExpEval s a <*> aExpEval s b
bExpEval s (Equal a b) = (==) <$> aExpEval s a <*> aExpEval s b
bExpEval s (NotEqual a b) = (/=) <$> aExpEval s a <*> aExpEval s b
```

# 5.2.3. Array Expressions Evaluation
The evaluation of an array expression takes as input a state and an array expression and gives as output a <code>Maybe (Array Int)</code> or <code>Nothing</code> depending on the success or failure of the computation.

```haskell
arrayExpEval :: State -> ArrayExp -> Maybe (Array Int)
arrayExpEval s (ArrayValues a) = if hasFailed 
                                    then Nothing
                                    else Just $ map (\v -> case v of Just x -> x) r
                                      where hasFailed = or $ map (\v -> case v of
                                              Nothing -> True
                                              Just x -> False) r
                                            r = map (\exp -> aExpEval s exp) a
arrayExpEval s (ArrayExpVariable v) = 
  case get s v of
    Just (IntegerType _) -> error "Assignment of an integer value to an array one not allowed!"
    Just (BooleanType _) -> error "Assignment of an boolean value to an array one not allowed!"
    Just (ArrayType a) -> Just a
    Nothing -> error "Variable to assign not found"
```

# 5.2.4. Commands Evaluation
The evaluation of a command takes a state as input (which represents an internal representation of the memory) and a list of commands (coded in the internal reprentation way) and gives <code>Just Bool</code> or <code>Nothing</code> depending of the success or failure of the computation.

The possible commands are the one listed above at the beginning of the documentation:
- Skip;
- AExpDeclaration;
- BExpDeclaration;
- ArrayDeclaration;
- AExpAssignment;
- BExpAssignment;
- ArrayAssignmentSingleValue;
- ArrayAssignmentValues;
- IfThenElse;
- While
```haskell
exeCommands :: State -> [Command] -> State
exeCommands s [] = s
exeCommands s (Skip : cs) = exeCommands s cs
exeCommands s ((AExpDeclaration v exp) : cs) =
  case aExpEval s exp of
    Just ex' -> case get s v of
                  Just _ -> error "Variable already declared!"
                  Nothing -> exeCommands (insert s v (IntegerType ex')) cs
    Nothing -> error "Invalid aExp"
exeCommands s ((BExpDeclaration v exp) : cs) =
  case bExpEval s exp of
    Just ex' -> case get s v of
                  Just _ -> error "Variable already declared!"
                  Nothing -> exeCommands (insert s v (BooleanType ex')) cs
    Nothing -> error "Invalid bExp"
exeCommands s ((ArrayDeclaration v exp) : cs) =
  case aExpEval s exp of
    Just ex' -> case get s v of
                  Just _ -> error "Variable already declared!"
                  Nothing -> exeCommands (insert s v (ArrayType a)) cs
                              where a = declare ex'
    Nothing -> error "Invalid size!"
exeCommands s ((AExpAssignment v exp) : cs) =
  case get s v of
    Just (IntegerType _) -> exeCommands (insert s v (IntegerType exp')) cs
                              where Just exp' = aExpEval s exp
    Just (BooleanType _) -> error "Assignment of a bExp value to an aExp variable not allowed!"
    Just (ArrayType _) -> error "Assignment of an array value to an aExp variable not allowed!"
    Nothing -> error "Undeclared variable!"
exeCommands s ((BExpAssignment v exp) : cs) =
  case get s v of
    Just (BooleanType _) -> exeCommands (insert s v (BooleanType exp')) cs
                              where Just exp' = bExpEval s exp
    Just (IntegerType _) -> error "Assignment of an aExp value to a bExp variable not allowed!"
    Just (ArrayType _) -> error "Assignment of an array value to an bExp variable not allowed!"
    Nothing -> error "Undeclared variable!"
exeCommands s ((ArrayAssignmentSingleValue v i exp) : cs) =
  case get s v of
    Just (ArrayType a) -> case aExpEval s exp of
                            Just r -> exeCommands (insert s v (ArrayType exp')) cs
                              where Just exp' = Array.write i' r a
                                                  where Just i' = aExpEval s i
                            Nothing -> error "The expression you want to assign is not valid!"
    Just (IntegerType _) -> error "Assignment of an aExp value to an array variable not allowed!"
    Just (BooleanType _) -> error "Assignment of an bExp value to an array variable not allowed!"
    Nothing -> error "Undeclared variable!"
exeCommands s ((ArrayAssignmentValues v exp) : cs) =
  case get s v of
    Just (ArrayType a) -> case arrayExpEval s exp of
                            Just b -> if length a == length b 
                                        then exeCommands (insert s v (ArrayType b)) cs
                                        else error "Length not valid!"
                            Nothing -> error "One of the aExp evaluation of the array you want to assign failed"
    Just (IntegerType _) -> error "Assignment of an aExp value to an array variable not allowed!"
    Just (BooleanType _) -> error "Assignment of an bExp value to an array variable not allowed!"
    Nothing -> error "Undeclared variable!"
exeCommands s ((IfThenElse b c c') : cs) =
  case bExpEval s b of
    Just True -> exeCommands s (c ++ cs)
    Just False -> exeCommands s (c' ++ cs)
    Nothing -> error "Invalid boolean expression!"
exeCommands s ((While b c) : cs) =
  case bExpEval s b of
    Just True -> exeCommands s (c ++ [While b c] ++ cs)
    Just False -> exeCommands s cs
    Nothing -> error "Invalid boolean expression!"
```
# 6. Running an example
To run the example program, navigate to the project directory folder, then open <code>ghci</code> and load the modules as following:
```
:l Main.hs Dictiornary.hs Grammar.hs Interpreter.hs Array.hs Parser.hs
```
Then type <code>main</code> and the program will execute!

If you want to modify the example program and write your own program, just edit the <code>source.txt</code> file.