module Parser where

import RubyParser

import Data.Text hiding (unlines)
import Data.Functor
import Control.Monad
import Text.ParserCombinators.Parsec

symbol :: Parser Expression
symbol = do
  char ':'
  n     <- name
  return (Symbol n)


quotedString :: Parser String
quotedString = do
  char '"'
  p <- many (noneOf "\"")
  char '"'
  return p

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


name :: Parser String
name = do
  first <- letter
  rest  <- many alphaNum
  return (first : rest)

globalVariable :: Parser Expression
globalVariable = do
  char '$'
  n <- name
  return (GlobalVariable n)

localVariable :: Parser Expression
localVariable = liftM Variable name

variable :: Parser Expression
variable = globalVariable <|> instanceVariable <|> localVariable

integer :: Parser Expression
integer = do
  n <- many1 digit
  return (Integer (readInt n))

literal :: Parser Expression
literal = integer <|> symbol

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

value :: Parser Expression
value = literal <|> (try expression) <|> (try assignment) <|> variable

expression :: Parser Expression
expression = do
  spaces
  e <- (try assignment) <|> variable <|> literal
  spaces
  eof
  return e

main :: IO ()
main = runTests (parseExpr expression)
