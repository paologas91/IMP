module Parser where

import Grammar ( AExp(..), BExp(..), Command(..), ArrayExp(..))

newtype Parser a = P (String -> Maybe (a, String))

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

instance Functor Parser where
  fmap g (P p) = P (\input -> case p input of
    Nothing -> Nothing
    Just (v, out) -> Just (g v, out))

instance Applicative Parser where
  pure v = P (\input -> Just (v, input))
  (P pg) <*> px = P (\input -> case pg input of
    Nothing -> Nothing
    --[(g, out)] -> case fmap g px of
      --(P p) -> p out)
    Just (g, out) -> p out where (P p) = fmap g px)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (P p) >>= f = P (\input -> case p input of
                         Nothing -> Nothing
                         Just (v, out) -> q out where (P q) = f v)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\input -> Nothing)
  -- <|> :: Parser a -> Parser a -> Parser b
  (P p) <|> (P q) = P (\input -> case p input of
                                  Nothing -> q input
                                  Just (v, out) -> Just (v, out))

spaces :: [Char]
spaces = ['\n', '\t', '\r', ' ']

digits :: [Char]
digits = ['0' .. '9']

lowercases :: [Char]
lowercases = ['a' .. 'z']

uppercases :: [Char]
uppercases = ['A' .. 'Z']

-- Reads the next character from the string given as input
readNext :: Parser Char
readNext = P (\input -> case input of
                      [] -> Nothing
                      (x:xs) -> Just (x,xs))

-- Checks if x satisfies a property given as input
sat :: (Char -> Bool) -> Parser Char
sat p = 
  do {
    x <- readNext;
    if p x then return x else empty;
  }

-- checks if a character is a space or not
isSpace :: Char -> Bool
isSpace c = c `elem` spaces

-- checks if a character is a digit or not
isDigit :: Char -> Bool
isDigit c = c `elem` digits

-- checks if a character is lowercase or not
isLowerCase :: Char -> Bool
isLowerCase c = c `elem` lowercases

-- checks if a character is uppercase or not
isUpperCase :: Char -> Bool
isUpperCase c = c `elem` uppercases

-- checks if a character is a letter or not
isLetter :: Char -> Bool
isLetter c = isUpperCase c || isLowerCase c

-- 
digitParser :: Parser Char
digitParser = sat isDigit

--
lowerParser :: Parser Char
lowerParser = sat isLowerCase

--
upperParser :: Parser Char
upperParser = sat isUpperCase

--
letterParser :: Parser Char
letterParser = sat isLetter

-- parses whitespaces removing them
spaceParser :: Parser ()
spaceParser = do many (sat isSpace)
                 return ()

-- parses a keyword by checking that every character read is equal to every character of the keyword given as input
keyword :: String -> Parser String
keyword [] = return []
keyword (x:xs) = do sat (== x)
                    keyword xs
                    return (x:xs)

--
identifier :: Parser String
identifier = do x <- letterParser
                xs <- many (letterParser <|> digitParser)
                return (x:xs)

-- 
naturalNumber :: Parser Int
naturalNumber = do xs <- some digitParser
                   return (read xs) -- read casts from string to integer
                -- oppure fmap read (some digitParser) oppure read <$> some digitParser

-- 
integerNumber :: Parser Int
integerNumber = do sat (== '-')
                   n <- naturalNumber
                   return (-n)
                <|>
                naturalNumber

-- 
token :: Parser a -> Parser a
token p = do spaceParser
             v <- p
             spaceParser
             return v

--
keywordParser :: String -> Parser String
keywordParser xs = token (keyword xs)

--
identifierParser :: Parser String
identifierParser = token identifier

--
naturalNumberParser :: Parser Int
naturalNumberParser = token naturalNumber

--
integerNumberParser :: Parser Int
integerNumberParser = token integerNumber

--
aExpParser :: Parser AExp
aExpParser = do chain aTermParser op
   where op = (do keywordParser "+"; return Add)
               <|> do keywordParser "-"; return Sub

--
aTermParser :: Parser AExp
aTermParser = do chain aFactParser op
               where op = do keywordParser "*"; return Mul

--
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

--
bExpParser :: Parser BExp
bExpParser = do chain bTermParser op 
               where op = do keywordParser "or"
                             return Or

--        
bTermParser :: Parser BExp
bTermParser = do chain bFactParser op
               where op = do keywordParser "and"
                             return And

--
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

--
variableDeclParser :: Parser Command
variableDeclParser = do {
   keywordParser "int";
   i <- identifierParser;
   keywordParser "=";
   a <- aExpParser;
   keywordParser ";";
   return (AExpDeclaration i a)
   } <|> do {
   keywordParser "bool";
   i <- identifierParser;
   keywordParser "<-";
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

--
skipParser :: Parser Command
skipParser = do keywordParser "skip"
                keywordParser ";"
                return Skip

--
assignmentParser :: Parser Command
assignmentParser = do i <- identifierParser
                      do keywordParser "="
                         a <- aExpParser
                         keywordParser ";"
                         return (AExpAssignment i a)
                       <|>
                       do keywordParser "<-"
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

--                    
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
                          return (IfThenElse b thenCase [Skip])

--
whileParser :: Parser Command
whileParser = do keywordParser "while"
                 keywordParser "("
                 b <- bExpParser
                 keywordParser ")"
                 keywordParser "{"
                 p <- programParser
                 keywordParser "}"
                 return (While b p)

--
commandParser :: Parser Command
commandParser = variableDeclParser
                <|> skipParser
                <|> assignmentParser
                <|> ifThenElseParser
                <|> whileParser

--
programParser :: Parser [Command]
programParser = do many commandParser

--
exeParser :: String -> ([Command], String)
exeParser s = case result of
                Nothing -> error "Parsed not executed"
                Just (cs, str) -> (cs, str)
              where (P p) = programParser
                    result = p s