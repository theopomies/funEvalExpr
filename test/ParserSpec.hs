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
