module LexerSpec (lexerSpec) where

import Control.Exception (evaluate)
import Error (EvalExprException (LexingException))
import Lexer (Token (CloseParenthesis, DivideSign, MinusSign, MultiplySign, Number, OpenParenthesis, PlusSign, PowerSign), tokenize)
import Test.Hspec (Spec, describe, it, shouldThrow)

instance Eq Token where
  (==) (Number n1) (Number n2) = n1 == n2
  (==) PlusSign PlusSign = True
  (==) MinusSign MinusSign = True
  (==) MultiplySign MultiplySign = True
  (==) DivideSign DivideSign = True
  (==) PowerSign PowerSign = True
  (==) OpenParenthesis OpenParenthesis = True
  (==) CloseParenthesis CloseParenthesis = True
  (==) _ _ = False

lexerSpec :: Spec
lexerSpec = describe "Test for lexer module: " $ do
  it "tokenizes a simple integer" $ do
    tokenize "42" == [Number 42]
  it "tokenizes a simple double" $ do
    tokenize "42.42" == [Number 42.42]
  it "tokenizes a longer double" $ do
    tokenize "428282.43422" == [Number 428282.43422]
  it "tokenizes a simple + sign" $ do
    tokenize "+" == [PlusSign]
  it "tokenizes a simple - sign" $ do
    tokenize "-" == [MinusSign]
  it "tokenizes a simple * sign" $ do
    tokenize "*" == [MultiplySign]
  it "tokenizes a simple / sign" $ do
    tokenize "/" == [DivideSign]
  it "tokenizes a simple ^ sign" $ do
    tokenize "^" == [PowerSign]
  it "tokenizes a simple ( sign" $ do
    tokenize "(" == [OpenParenthesis]
  it "tokenizes a simple ) sign" $ do
    tokenize ")" == [CloseParenthesis]
  it "tokenizes a simple addition" $ do
    tokenize "1+1" == [Number 1, PlusSign, Number 1]
  it "tokenizes a simple addition with random spaces" $ do
    tokenize "  1   + 1" == [Number 1, PlusSign, Number 1]
  it "tokenizes a random operation" $ do
    tokenize "(1 + 2) / 5 + 3.24 ^ 3.5 * 42" == [OpenParenthesis, Number 1, PlusSign, Number 2, CloseParenthesis, DivideSign, Number 5, PlusSign, Number 3.24, PowerSign, Number 3.5, MultiplySign, Number 42]
  it "tokenizes the subjects operation" $ do
    tokenize "(0.345+ 5 )*( -2-1) / 3" == [OpenParenthesis, Number 0.345, PlusSign, Number 5, CloseParenthesis, MultiplySign, OpenParenthesis, MinusSign, Number 2, MinusSign, Number 1, CloseParenthesis, DivideSign, Number 3]

--   it "Invalid random expression" $ do
--     evaluate (tokenize "certes oui oui") `shouldThrow` (== LexingException "A value entered was not a valid double.")