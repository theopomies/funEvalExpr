module ArgsSpec (argsSpec) where

import Args (ExpressionString (ExpressionString), parseArgs)
import Control.Exception (evaluate)
import Error (EvalExprException (HelpException, InvalidArgsException))
import Test.Hspec (Spec, describe, it, shouldThrow)

instance Eq ExpressionString where
  (==) (ExpressionString lhs) (ExpressionString rhs) = lhs == rhs

argsSpec :: Spec
argsSpec = describe "Test for args module: " $ do
  it "should recognize a singe arg" $ do
    parseArgs ["1+1"] == ExpressionString "1+1"
  it "should recognize a singe arg even if not an expression" $ do
    parseArgs ["def not an expression"] == ExpressionString "def not an expression"
  it "should recognize a simple help flag" $ do
    evaluate (parseArgs ["-h"]) `shouldThrow` (== HelpException)
  it "should recognize a simple help flag even if other args appear after" $ do
    evaluate (parseArgs ["-h", "this", "works"]) `shouldThrow` (== HelpException)
  it "should recognize a long help flag" $ do
    evaluate (parseArgs ["--help"]) `shouldThrow` (== HelpException)
  it "should recognize a long help flag even if other args appear after" $ do
    evaluate (parseArgs ["--help", "this", "works"]) `shouldThrow` (== HelpException)
  it "should throw on empty args" $ do
    evaluate (parseArgs []) `shouldThrow` (== InvalidArgsException)
  it "should throw on too many args" $ do
    evaluate (parseArgs ["1+1", "this", "doesnt", "work"]) `shouldThrow` (== InvalidArgsException)