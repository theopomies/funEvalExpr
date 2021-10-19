module Parser (Expression (..), TermBlock (..), FactorBlock (..), PowerBlock (..), parse) where

import Error (EvalExprException (ParsingException))
import GHC.Exception (throw)
import Lexer (Token (..), Tokens)

data Expression
  = Expression TermBlock
  | Add TermBlock TermBlock
  | Sub TermBlock TermBlock

data TermBlock
  = TermBlock FactorBlock
  | Mul FactorBlock FactorBlock
  | Div FactorBlock FactorBlock

data FactorBlock
  = FactorBlock PowerBlock
  | Pow PowerBlock FactorBlock

data PowerBlock
  = PowerBlock Expression
  | Parenthesised Expression
  | Value Double
  | Neg Expression
  | Pos Expression

parse :: Tokens -> Expression
parse tokens
  | null (snd . parseExpression $ tokens) = fst $ parseExpression tokens
  | otherwise = throw $ ParsingException "The expression provided was invalid."

parseExpression :: Tokens -> (Expression, Tokens)
parseExpression = parseExpression' . parseTermBlock

parseExpression' :: (TermBlock, Tokens) -> (Expression, Tokens)
parseExpression' (firstTerm, PlusSign : xs) = let (secondTerm, tokens) = parseTermBlock xs in parseExpression' (TermBlock . FactorBlock . PowerBlock $ Add firstTerm secondTerm, tokens)
parseExpression' (firstTerm, MinusSign : xs) = let (secondTerm, tokens) = parseTermBlock xs in parseExpression' (TermBlock . FactorBlock . PowerBlock $ Sub firstTerm secondTerm, tokens)
parseExpression' (term, tokens) = (Expression term, tokens)

parseTermBlock :: Tokens -> (TermBlock, Tokens)
parseTermBlock = parseTermBlock' . parseFactorBlock

parseTermBlock' :: (FactorBlock, Tokens) -> (TermBlock, Tokens)
parseTermBlock' (firstFactor, MultiplySign : xs) = let (secondFactor, tokens) = parseFactorBlock xs in parseTermBlock' (FactorBlock . PowerBlock . Expression $ Mul firstFactor secondFactor, tokens)
parseTermBlock' (firstFactor, DivideSign : xs) = let (secondFactor, tokens) = parseFactorBlock xs in parseTermBlock' (FactorBlock . PowerBlock . Expression $ Div firstFactor secondFactor, tokens)
parseTermBlock' (factor, tokens) = (TermBlock factor, tokens)

parseFactorBlock :: Tokens -> (FactorBlock, Tokens)
parseFactorBlock = parseFactorBlock' . parsePowerBlock

parseFactorBlock' :: (PowerBlock, Tokens) -> (FactorBlock, Tokens)
parseFactorBlock' (firstPower, PowerSign : xs) = let (secondPower, tokens) = parseFactorBlock xs in parseFactorBlock' (PowerBlock . Expression . TermBlock $ Pow firstPower secondPower, tokens)
parseFactorBlock' (factor, tokens) = (FactorBlock factor, tokens)

parsePowerBlock :: Tokens -> (PowerBlock, Tokens)
parsePowerBlock ((Number n) : xs) = (Value n, xs)
parsePowerBlock (OpenParenthesis : xs)
  | null (snd . parseExpression $ xs) = throw $ ParsingException "An open parenthesis was not closed."
  | head (snd . parseExpression $ xs) == CloseParenthesis = let (expression, restTokens) = parseExpression xs in (Parenthesised expression, tail restTokens)
  | otherwise = throw $ ParsingException "An open parenthesis was not closed."
parsePowerBlock (MinusSign : xs) = let (factor, restTokens) = parseFactorBlock xs in (Neg . Expression . TermBlock $ factor, restTokens)
parsePowerBlock (PlusSign : xs) = let (factor, restTokens) = parseFactorBlock xs in (PowerBlock . Expression . TermBlock $ factor, restTokens)
parsePowerBlock _ = throw $ ParsingException "The expression provided was invalid."
