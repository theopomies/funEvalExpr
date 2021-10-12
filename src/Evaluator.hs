module Evaluator where

import Parser (ExpressionTree(..), BinaryOperation(..), UnaryOperation(..))
import Data.Char (GeneralCategory(ModifierSymbol))

evaluate :: ExpressionTree -> Double
evaluate (Leaf num) = num
evaluate (BinaryNode Add lhs rhs) = evaluate lhs + evaluate rhs
evaluate (BinaryNode Sub lhs rhs) = evaluate lhs - evaluate rhs
evaluate (BinaryNode Mul lhs rhs) = evaluate lhs * evaluate rhs
evaluate (BinaryNode Div lhs rhs) = evaluate lhs / evaluate rhs
evaluate (BinaryNode Pow lhs rhs) = evaluate lhs ** evaluate rhs
evaluate (UnaryNode Neg rhs) = -(evaluate rhs)
evaluate (UnaryNode Pos rhs) = evaluate rhs
evaluate (ProtectedNode tree) = evaluate tree
