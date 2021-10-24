module E2ESpec (e2eSpec) where

import Args (ExpressionString (ExpressionString))
import Evaluator (evaluate)
import Lexer (tokenize)
import Parser (parse)
import Test.Hspec (Spec, describe, it, shouldThrow)

evalExpr :: String -> Double
evalExpr = evaluate . parse . tokenize . ExpressionString

e2eSpec :: Spec
e2eSpec = describe "~End-to-end tests:" $ do
  it "should compute a single number" $ do
    evalExpr "1" == 1
  it "should compute a single unary + and number" $ do
    evalExpr "+1" == 1
  it "should compute a single unary + and number spaced" $ do
    evalExpr "+ 1" == 1
  it "should compute a single unary + and number with digits" $ do
    evalExpr "+1.1" == 1.1
  it "should compute a single unary - and number" $ do
    evalExpr "-1" == -1
  it "should compute a single unary - and number spaced" $ do
    evalExpr "- 1" == -1
  it "should compute a single unary - and number with digits" $ do
    evalExpr "-1.1" == -1.1
  it "should compute a sigle binary operation +" $ do
    evalExpr "1+1" == 2
  it "should compute a sigle binary operation -" $ do
    evalExpr "1-1" == 0
  it "should compute a sigle binary operation /" $ do
    evalExpr "2/2" == 1
  it "should compute a sigle binary operation *" $ do
    evalExpr "2*2" == 4
  it "should compute a sigle binary operation ^" $ do
    evalExpr "2^3" == 8
  it "should understand priorities" $ do
    evalExpr "2^3+1" == 9
  it "should understand priorities 2" $ do
    evalExpr "2-1*2" == 0
  it "should understand priorities 3" $ do
    evalExpr "2-1+1" == 2
  it "should understand priorities 4" $ do
    evalExpr "2/2-2" == -1
  it "should understand priorities 5" $ do
    evalExpr "2^1^2" == 2
  it "should compute complex expressions" $ do
    evalExpr "2/(2-1)*3" == 6
  it "should compute complex expressions 2" $ do
    evalExpr "(2+2*2+1-2+5)^2" == 100
  it "should compute complex expressions 3" $ do
    evalExpr "1+2*3^2*2+2-4^5" == -985
  it "should compute complex expressions 4" $ do
    evalExpr "1*2+(1+(3^(6/3)))" == 12
  it "should compute complex expressions 5" $ do
    evalExpr "3--2" == 5
  it "should compute complex expressions 6" $ do
    evalExpr "5++6-+1" == 10
