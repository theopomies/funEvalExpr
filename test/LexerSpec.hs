module LexerSpec (lexerSpec) where

import Args (ExpressionString (ExpressionString))
import Control.Exception (evaluate)
import Lexer (Token (CloseParenthesis, DivideSign, MinusSign, MultiplySign, Number, OpenParenthesis, PlusSign, PowerSign), tokenize)
import Test.Hspec (Spec, describe, it, shouldThrow)

lexerSpec :: Spec
lexerSpec = describe "Test for lexer module: " $ do
  it "tokenizes a simple integer" $ do
    tokenize (ExpressionString "42") == [Number 42]
  it "tokenizes a simple double" $ do
    tokenize (ExpressionString "42.42") == [Number 42.42]
  it "tokenizes a longer double" $ do
    tokenize (ExpressionString "428282.43422") == [Number 428282.43422]
  it "tokenizes a simple + sign" $ do
    tokenize (ExpressionString "+") == [PlusSign]
  it "tokenizes a simple - sign" $ do
    tokenize (ExpressionString "-") == [MinusSign]
  it "tokenizes a simple * sign" $ do
    tokenize (ExpressionString "*") == [MultiplySign]
  it "tokenizes a simple / sign" $ do
    tokenize (ExpressionString "/") == [DivideSign]
  it "tokenizes a simple ^ sign" $ do
    tokenize (ExpressionString "^") == [PowerSign]
  it "tokenizes a simple ( sign" $ do
    tokenize (ExpressionString "(") == [OpenParenthesis]
  it "tokenizes a simple ) sign" $ do
    tokenize (ExpressionString ")") == [CloseParenthesis]
  it "tokenizes a simple addition" $ do
    tokenize (ExpressionString "1+1") == [Number 1, PlusSign, Number 1]
  it "tokenizes a simple addition with random spaces" $ do
    tokenize (ExpressionString "  1   + 1") == [Number 1, PlusSign, Number 1]
  it "tokenizes a random operation" $ do
    tokenize (ExpressionString "(1 + 2) / 5 + 3.24 ^ 3.5 * 42") == [OpenParenthesis, Number 1, PlusSign, Number 2, CloseParenthesis, DivideSign, Number 5, PlusSign, Number 3.24, PowerSign, Number 3.5, MultiplySign, Number 42]
  it "tokenizes the subjects operation" $ do
    tokenize (ExpressionString "(0.345+ 5 )*( -2-1) / 3") == [OpenParenthesis, Number 0.345, PlusSign, Number 5, CloseParenthesis, MultiplySign, OpenParenthesis, MinusSign, Number 2, MinusSign, Number 1, CloseParenthesis, DivideSign, Number 3]
