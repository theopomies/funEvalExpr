module Evaluator (evaluate) where

import Control.Exception.Base (throw)
import Error (EvalExprException (ComputingException))
import Parser (Expression (..), Factor (..), Power (..), TermBlock (..))

evaluate :: Expression -> Double
evaluate (Add lhs rhs) = evaluateTerm lhs + evaluateTerm rhs
evaluate (Sub lhs rhs) = evaluateTerm lhs - evaluateTerm rhs
evaluate (Expression rhs) = evaluateTerm rhs

evaluateTerm :: TermBlock -> Double
evaluateTerm (TermBlock term) = evaluateFactor term
evaluateTerm (Mul lhs rhs) = evaluateFactor lhs * evaluateFactor rhs
evaluateTerm (Div lhs rhs)
  | evaluateFactor rhs == 0 = throw $ ComputingException "Cannot divide by 0"
  | otherwise = evaluateFactor lhs / evaluateFactor rhs

evaluateFactor :: Factor -> Double
evaluateFactor (Factor factor) = evaluatePower factor
evaluateFactor (Pow lhs rhs) = evaluatePower lhs ** evaluateFactor rhs

evaluatePower :: Power -> Double
evaluatePower (Value val) = val
evaluatePower (Power expr) = evaluate expr
evaluatePower (Parenthesised expr) = evaluate expr
evaluatePower (Neg expr) = - evaluate expr
evaluatePower (Pos expr) = evaluate expr