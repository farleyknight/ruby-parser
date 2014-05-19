module RubyParser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Test.Hspec
import Test.QuickCheck

import Text.ParserCombinators.Parsec.Error
  (ParseError, Message, errorMessages, messageEq)

data Expression = Assignment Expression Expression
                | Variable String
                | InstanceVariable String
                | GlobalVariable String
                | Symbol String
                | Integer Int
                | Comparision String Expression Expression
                deriving (Show, Read, Eq)

instance Arbitrary Expression where
  arbitrary = do
    n <- arbitrary
    return (Symbol n)

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

parseExpr parser input = parse parser "" input

should :: Example a => String -> a -> Spec
should x = it ("should " ++ x)

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

runTests parseExpr = hspec $ do
  describe "parseExpr" $ do
    it "is the opposite of unparseExpr" $ property $
      \x -> (read (show x)) == (x :: Expression)
    should "throw an error if trying to assign a variable to a fixnum" $ do
      (show $ parseExpr "1 = a") `shouldBe`
        (errorMessageFor 1 3 "'='" "space or end of input")
    should "throw an error if trying to assign a fixnum to a symbol" $ do
      (show $ parseExpr ":foo = 1") `shouldBe`
        (errorMessageFor 1 6 "'='" "space or end of input")
    context "composed with unparseExpr" $ do
      should "parse and unparse 'a = 1'" $ do
        (unparseExpr (parseExpr "a = 1")) `shouldBe` (Right "a = 1")
      should "parse and unparse 'a = b = 1'" $ do
        (unparseExpr (parseExpr "a = b = 1")) `shouldBe` (Right "a = b = 1")
