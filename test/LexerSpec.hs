module LexerSpec (lexerSpec) where

import Control.Exception (evaluate)
import Error (EvalExprException (LexingException))
import Lexer (tokenize)
import Test.Hspec (Spec, describe, it, shouldThrow)

lexerSpec :: Spec
lexerSpec = describe "Test for lexer module: " $ do
  it "Invalid random expression" $ do
    evaluate (tokenize "certes oui oui") `shouldThrow` (== LexingException "A value entered was not a valid double.")
