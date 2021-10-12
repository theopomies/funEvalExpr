module Parser where

data BinaryOperation = Add | Sub | Mul | Div | Pow

data UnaryOperation = Neg | Pos

data ExpressionTree
  = Leaf Double
  | BinaryNode BinaryOperation ExpressionTree ExpressionTree
  | UnaryNode UnaryOperation ExpressionTree
  | ProtectedNode ExpressionTree
