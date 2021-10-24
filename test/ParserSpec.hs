module ParserSpec (parserSpec) where

import Lexer (Token (..), Tokens)
import Parser (Expression (..), FactorBlock (..), PowerBlock (..), TermBlock (..), parse)
import Test.Hspec (Spec, describe, it, shouldThrow)

instance Eq Expression where
  (==) (Add lhs rhs) (Add lhs' rhs') = lhs == lhs' && rhs == rhs'
  (==) (Sub lhs rhs) (Sub lhs' rhs') = lhs == lhs' && rhs == rhs'
  (==) (Expression termBlock) (Expression termBlock') = termBlock == termBlock'
  (==) _ _ = False

instance Eq TermBlock where
  (==) (Div lhs rhs) (Div lhs' rhs') = lhs == lhs' && rhs == rhs'
  (==) (Mul lhs rhs) (Mul lhs' rhs') = lhs == lhs' && rhs == rhs'
  (==) (TermBlock factorBlock) (TermBlock factorBlock') = factorBlock == factorBlock'
  (==) _ _ = False

instance Eq FactorBlock where
  (==) (Pow lhs rhs) (Pow lhs' rhs') = lhs == lhs' && rhs == rhs'
  (==) (FactorBlock powerBlock) (FactorBlock powerBlock') = powerBlock == powerBlock'
  (==) _ _ = False

instance Eq PowerBlock where
  (==) (Pos lhs) (Pos lhs') = lhs == lhs'
  (==) (Neg lhs) (Neg lhs') = lhs == lhs'
  (==) (Value lhs) (Value lhs') = lhs == lhs'
  (==) (Parenthesised lhs) (Parenthesised lhs') = lhs == lhs'
  (==) (PowerBlock powerBlock) (PowerBlock powerBlock') = powerBlock == powerBlock'
  (==) _ _ = False

parserSpec :: Spec
parserSpec = describe "Parser module tests:" $ do
  it "should parse a single number" $ do
    parse [Number 1] == (Expression . TermBlock . FactorBlock . Value $ 1)
  it "should parse a single addition" $ do
    parse [Number 1, PlusSign, Number 1] == Add (TermBlock . FactorBlock . Value $ 1) (TermBlock . FactorBlock . Value $ 1)
  it "parses a simple substraction" $ do
    parse [Number 1, MinusSign, Number 1] == Sub (TermBlock . FactorBlock . Value $ 1.00) (TermBlock . FactorBlock . Value $ 1.00)
  it "parses a simple multiplication" $ do
    parse [Number 5, MultiplySign, Number 5] == Expression (Mul (FactorBlock . Value $ 5.00) (FactorBlock . Value $ 5.00))
  it "parses a simple division" $ do
    parse [Number 25, DivideSign, Number 5] == Expression (Div (FactorBlock . Value $ 25.00) (FactorBlock . Value $ 5.00))
  it "parses a simple power" $ do
    parse [Number 5, PowerSign, Number 2] == (Expression . TermBlock $ Pow (Value 5.00) (FactorBlock . Value $ 2))
  it "parses a parenthesised expression" $ do
    parse [OpenParenthesis, Number 3, CloseParenthesis] == (Expression . TermBlock . FactorBlock $ Parenthesised (Expression . TermBlock . FactorBlock . Value $ 3.00))
  it "parses a simple addition with a multiplication 2 * 2 + 2" $ do
    parse [Number 2, MultiplySign, Number 2, PlusSign, Number 2] == Add (Mul (FactorBlock . Value $ 2.00) (FactorBlock . Value $ 2.00)) (TermBlock . FactorBlock . Value $ 2.00)
  it "parses a parenthesised addition to the power of -> (1 + 1) ^ 2" $ do
    parse [OpenParenthesis, Number 1, PlusSign, Number 1, CloseParenthesis, PowerSign, Number 2] == (Expression . TermBlock $ Pow (Parenthesised (Add (TermBlock . FactorBlock . Value $ 1.00) (TermBlock . FactorBlock . Value $ 1.00))) (FactorBlock . Value $ 2.00))
  it "parses a value to the power of an addition -> 2 ^ ( 1 + 1 )" $ do
    parse [Number 2, PowerSign, OpenParenthesis, Number 1, PlusSign, Number 1, CloseParenthesis] == (Expression . TermBlock $ Pow (Value 2.00) (FactorBlock $ Parenthesised (Add (TermBlock . FactorBlock . Value $ 1.00) (TermBlock . FactorBlock . Value $ 1.00))))
  it "parses a division by a number to the power of -> 4 / (2 ^ 2)" $ do
    parse [Number 4, DivideSign, OpenParenthesis, Number 2, PowerSign, Number 2, CloseParenthesis] == Expression (Div (FactorBlock . Value $ 4.00) (FactorBlock $ Parenthesised (Expression . TermBlock $ Pow (Value 2.00) (FactorBlock . Value $ 2.00))))
  it "parses a negated expression" $ do
    parse [MinusSign, Number 5] == (Expression . TermBlock . FactorBlock $ Neg (Expression . TermBlock . FactorBlock . Value $ 5.00))
  it "parses a negated power" $ do
    parse [MinusSign, Number 5, PowerSign, Number 2] == (Expression . TermBlock . FactorBlock $ Neg (Expression . TermBlock $ Pow (Value 5.00) (FactorBlock . Value $ 2)))
  it "parses a number to the power of a division -> 25 ^ (1 / 2)" $ do
    parse [Number 25, PowerSign, OpenParenthesis, Number 1, DivideSign, Number 2, CloseParenthesis] == (Expression . TermBlock $ Pow (Value 25.00) (FactorBlock $ Parenthesised (Expression $ Div (FactorBlock . Value $ 1.00) (FactorBlock . Value $ 2.00))))
