module Parser where

import Text.ParserCombinators.Parsec.Error
  (ParseError, Message, errorMessages, messageEq)

import Test.Hspec
import Data.Text hiding (unlines)
import Data.Functor
import Control.Monad
import Text.ParserCombinators.Parsec

data Expression = Assignment Expression Expression
                | Variable String
                | InstanceVariable String
                | GlobalVariable String
                | Symbol String
                | Integer Int
                deriving Show

-- data Value =

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

--
symbol :: Parser Expression
symbol = do
  char ':'
  n     <- name
  return (Symbol n)

symbol' :: Parser Expression
symbol' = liftM Symbol $ (char ':') *> name

symbol'' :: Parser Expression
symbol'' = Symbol <$> ((char ':') *> name)

quotedString :: Parser String
quotedString = do
  char '"'
  p <- many (noneOf "\"")
  char '"'
  return p

quotedString' :: Parser String
quotedString' = (char '"') *> (many (noneOf "\"")) <* (char '"')

(<*>) :: Monad m => m t1 -> m t2 -> m (t1, t2)
a <*> b = do
  x <- a
  y <- b
  return (x, y)

(<*) :: Monad m => m t1 -> m t2 -> m t1
a <* b = do
  x <- a
  _ <- b
  return x

(*>) :: Monad m => m t1 -> m t2 -> m t2
a *> b = do
  _ <- a
  y <- b
  return y

readInt :: String -> Int
readInt x = (read x :: Int)

instanceVariable :: Parser Expression
instanceVariable = do
  char '@'
  n <- name
  return (InstanceVariable n)

instanceVariable' :: Parser Expression
instanceVariable' = (liftM InstanceVariable) ((char '@') *> name)

instanceVariable'' :: Parser Expression
instanceVariable'' = InstanceVariable <$> ((char '@') *> name)

name :: Parser String
name = do
  first <- letter
  rest  <- many alphaNum
  return (first : rest)

name' :: Parser String
name' = liftM (uncurry (:)) (letter <*> (many alphaNum))

name'' :: Parser String
name'' = (uncurry (:)) <$> (letter <*> (many alphaNum))

globalVariable :: Parser Expression
globalVariable = do
  char '$'
  n <- name
  return (GlobalVariable n)

globalVariable' :: Parser Expression
globalVariable' = liftM GlobalVariable $ (char '$') *> name

globalVariable'' :: Parser Expression
globalVariable'' = GlobalVariable <$> ((char '$') *> name)

localVariable :: Parser Expression
localVariable = liftM Variable name

localVariable' :: Parser Expression
localVariable' = Variable <$> name

variable :: Parser Expression
variable = globalVariable <|> instanceVariable <|> localVariable

variable' :: Parser Expression
variable' = globalVariable' <|> instanceVariable' <|> localVariable'

integer :: Parser Expression
integer = do
  n <- many1 digit
  return (Integer (readInt n))

integer' :: Parser Expression
integer' = liftM (Integer . readInt) (many1 digit)

integer'' :: Parser Expression
integer'' = (Integer . readInt) <$> (many1 digit)

literal :: Parser Expression
literal = integer <|> symbol

literal' :: Parser Expression
literal' = integer' <|> symbol'

padded :: Parser a -> Parser ()
padded f = (spaces >> f >> spaces)

equalsSign :: Parser Char
equalsSign = char '='

assignment :: Parser Expression
assignment = do
  var <- variable
  spaces
  char '='
  spaces
  val <- (try assignment) <|> literal <|> variable
  return (Assignment var val)

assignment' :: Parser Expression
assignment' = liftM (uncurry Assignment)
   (variable <* (spaces >> equalsSign >> spaces) <*> value)

assignment'' :: Parser Expression
assignment'' =
  (uncurry Assignment) <$> (variable' <* (padded equalsSign) <*> value')

value :: Parser Expression
value = literal <|> (try expression) <|> (try assignment) <|> variable

value' :: Parser Expression
value' = literal' <|> (try expression') <|> (try assignment) <|> variable'

expression :: Parser Expression
expression = do
  spaces
  e <- (try assignment) <|> variable <|> literal
  spaces
  eof
  return e

expression' :: Parser Expression
expression' = spaces *> statement <* (spaces >> eof)
  where
    statement = (try assignment') <|> literal' <|> variable' <|> value'

parseExpr input = parse expression' "" input

parseExprWithParser input parser = parse parser "" input

should :: Example a => String -> a -> Spec
should x = it ("should" ++ x)

errorMessageFor line column unexpected expecting =
  "Left (line " ++ (show line) ++ ", column " ++ (show column) ++ "):\n" ++
  "unexpected " ++ unexpected ++ "\n" ++
  "expecting " ++ expecting


unparse :: Expression -> String
unparse expr =
  case expr of
    Assignment e1 e2   -> (unparse e1) ++ " = " ++ (unparse e2)
    Variable v         -> v
    InstanceVariable v -> "@" ++ v
    GlobalVariable v   -> "$" ++ v
    Symbol s           -> ":" ++ s
    Integer i          -> show i

unparseExpr :: Monad m => m Expression -> m String
unparseExpr = liftM unparse

main :: IO ()
main = hspec $ do
  describe "parseExpr" $ do
    it "should parse and unparse 'a = 1'" $ do
      (unparseExpr (parseExpr "a = 1")) `shouldBe` (Right "a = 1")
    it "should throw an error if trying to assign a variable to a fixnum" $ do
      (show $ parseExpr "1 = a") `shouldBe`
        (errorMessageFor 1 3 "'='" "space or end of input")
    it "should throw an error if trying to assign a fixnum to a symbol" $ do
      (show $ parseExpr ":foo = 1") `shouldBe`
        (errorMessageFor 1 6 "'='" "space or end of input")
