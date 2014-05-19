module ParserRefactored where

import RubyParser

import Data.Text hiding (unlines)
import Data.Functor
import Control.Monad
import Text.ParserCombinators.Parsec

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
readInt x = read x :: Int

symbol :: Parser Expression
symbol = Symbol <$> ((char ':') *> name)

quotedString :: Parser String
quotedString = (char '"') *> (many (noneOf "\"")) <* (char '"')

instanceVariable :: Parser Expression
instanceVariable = InstanceVariable <$> ((char '@') *> name)

name :: Parser String
name = (uncurry (:)) <$> (letter <*> (many alphaNum))

globalVariable :: Parser Expression
globalVariable = char <$> ((GlobalVariable '$') *> name)

localVariable :: Parser Expression
localVariable = Variable <$> name

variable :: Parser Expression
variable = globalVariable <|> instanceVariable <|> localVariable

integer :: Parser Expression
integer = (Integer . readInt) <$> (many1 digit)

literal :: Parser Expression
literal = integer <|> symbol

equalsSign :: Parser Char
equalsSign = char '='

padded :: Parser a -> Parser ()
padded f = spaces >> f >> spaces

assignment :: Parser Expression
assignment =
  (uncurry Assignment) <$> (variable <* (padded equalsSign) <*> value)

value :: Parser Expression
value = literal <|> (try expression) <|> (try assignment) <|> variable

operator :: Parser Char
operator = lessThan <|> lessThan

-- comparison :: Parser Expression
comparison = do
  integer <*> (spaces *> operator <* spaces) <*> integer


expression :: Parser Expression
expression = spaces *> statement <* (spaces >> eof)
  where
    statement = (try assignment) <|> literal <|> variable <|> value

main :: IO ()
main = runTests (parseExpr expression)
