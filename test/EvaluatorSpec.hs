module EvaluatorSpec ( evaluatorSpec ) where

import Test.Hspec (Spec, describe, it, shouldThrow)
import Evaluator (evaluate)
import qualified Control.Exception as CE (evaluate)
import Parser(Expression (..), TermBlock(..), FactorBlock(..), PowerBlock(..))
import Error (EvalExprException(ComputingException))

evaluatorSpec :: Spec
evaluatorSpec = describe "Test for evaluator module : " $ do
    it "evaluates a simple tree" $ do
        evaluate ( Expression . TermBlock . FactorBlock . Value $ 42.00 ) == 42.00
    it "evaluates a simple addition tree" $ do
        evaluate ( Add ( TermBlock . FactorBlock . Value $ 1.00 )( TermBlock . FactorBlock . Value $ 1.00 )) == 2.00
    it "evaluates a division by 0" $ do
        CE.evaluate (evaluate ( Expression $ Div ( FactorBlock . Value $ 1.00 )( FactorBlock . Value $ 0 ))) `shouldThrow` (== ComputingException "Cannot divide by 0")
    it "evaluates a simple substraction tree" $ do
        evaluate ( Sub ( TermBlock . FactorBlock . Value $ 1.00 )( TermBlock . FactorBlock . Value $ 1.00 )) == 0
    it "evaluates a simple multiplication tree" $ do
        evaluate ( Expression $ Mul ( FactorBlock . Value $ 5.00 )( FactorBlock . Value $ 5.00)) == 25.00
    it "evaluates a simple division tree" $ do
        evaluate ( Expression $ Div ( FactorBlock . Value $ 25.00 )( FactorBlock . Value $ 5.00)) == 5.00
    it "evaluates a simple power tree" $ do
        evaluate (Expression . TermBlock $ Pow ( Value 5.00 )( FactorBlock . Value $ 2 )) == 25.00
    it "evaluates a parenthesised tree" $ do
        evaluate ( Expression . TermBlock . FactorBlock $ Parenthesised ( Expression . TermBlock . FactorBlock . Value $ 3.00 )) == 3.00
    it "evaluates a simple addition tree with a multiplication tree 2 * 2 + 2" $ do
        evaluate ( Add ( Mul ( FactorBlock . Value $ 2.00 )( FactorBlock . Value $ 2.00 ))( TermBlock . FactorBlock . Value $ 2.00 )) == 6.00
    it "evaluates a parenthesised addition to the power of -> (1 + 1) ^ 2" $ do
        evaluate ( Expression . TermBlock $ Pow ( Parenthesised ( Add ( TermBlock . FactorBlock . Value $ 1.00 )( TermBlock . FactorBlock . Value $ 1.00 )))( FactorBlock . Value $ 2.00 )) == 4.00
    it "evaluates a value to the power of an addition tree -> 2 ^ ( 1 + 1 )" $ do
        evaluate ( Expression . TermBlock $ Pow ( Value 2.00 )( FactorBlock $ Parenthesised ( Add ( TermBlock . FactorBlock . Value $ 1.00 )( TermBlock . FactorBlock . Value $ 1.00 )))) == 4.00
    it "evaluates a division by a number to the power of -> 4 / (2 ^ 2)" $ do
        evaluate ( Expression $ Div ( FactorBlock . Value $ 4.00 )( FactorBlock $ Parenthesised ( Expression . TermBlock $ Pow ( Value 2.00 )(FactorBlock . Value $ 2.00 )))) == 1.00
    it "evaluates a negated expression tree" $ do
        evaluate ( Expression . TermBlock . FactorBlock $ Neg ( Expression . TermBlock . FactorBlock . Value $ - 5.00 )) == 5.00
    it "evaluates a negated power tree" $ do
        evaluate ( Expression . TermBlock . FactorBlock $ Neg ( Expression . TermBlock $ Pow ( Value 5.00 )( FactorBlock . Value $ 2 ))) == -25.00
    it "evaluates a number to the power of a division -> 25 ^ (1 / 2)" $ do
        evaluate ( Expression . TermBlock $ Pow ( Value 25.00 )( FactorBlock $ Parenthesised ( Expression $ Div ( FactorBlock . Value $ 1.00 )( FactorBlock . Value $ 2.00 )))) == 5.00
