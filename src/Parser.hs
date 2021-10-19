module Parser (Expression (..), TermBlock (..), Factor (..), Power (..), parse) where

import Error (EvalExprException (ParsingException))
import GHC.Exception (throw)
import Lexer (Token (..), Tokens)

data Expression
  = Expression TermBlock
  | Add TermBlock TermBlock
  | Sub TermBlock TermBlock

data TermBlock
  = TermBlock Factor
  | Mul Factor Factor
  | Div Factor Factor

data Factor
  = Factor Power
  | Pow Power Factor

data Power
  = Power Expression
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
parseExpression' (firstTerm, PlusSign : xs) = let (secondTerm, tokens) = parseTermBlock xs in parseExpression' (TermBlock . Factor . Power $ Add firstTerm secondTerm, tokens)
parseExpression' (firstTerm, MinusSign : xs) = let (secondTerm, tokens) = parseTermBlock xs in parseExpression' (TermBlock . Factor . Power $ Sub firstTerm secondTerm, tokens)
parseExpression' (term, tokens) = (Expression term, tokens)

parseTermBlock :: Tokens -> (TermBlock, Tokens)
parseTermBlock = parseTermBlock' . parseFactor

parseTermBlock' :: (Factor, Tokens) -> (TermBlock, Tokens)
parseTermBlock' (firstFactor, MultiplySign : xs) = let (secondFactor, tokens) = parseFactor xs in parseTermBlock' (Factor . Power . Expression $ Mul firstFactor secondFactor, tokens)
parseTermBlock' (firstFactor, DivideSign : xs) = let (secondFactor, tokens) = parseFactor xs in parseTermBlock' (Factor . Power . Expression $ Div firstFactor secondFactor, tokens)
parseTermBlock' (factor, tokens) = (TermBlock factor, tokens)

parseFactor :: Tokens -> (Factor, Tokens)
parseFactor = parseFactor' . parsePower

parseFactor' :: (Power, Tokens) -> (Factor, Tokens)
parseFactor' (firstPower, PowerSign : xs) = let (secondPower, tokens) = parseFactor xs in parseFactor' (Power . Expression . TermBlock $ Pow firstPower secondPower, tokens)
parseFactor' (factor, tokens) = (Factor factor, tokens)

parsePower :: Tokens -> (Power, Tokens)
parsePower ((Number n) : xs) = (Value n, xs)
parsePower (OpenParenthesis : xs)
  | null (snd . parseExpression $ xs) = throw $ ParsingException "An open parenthesis was not closed."
  | head (snd . parseExpression $ xs) == CloseParenthesis = let (expression, restTokens) = parseExpression xs in (Parenthesised expression, tail restTokens)
  | otherwise = throw $ ParsingException "An open parenthesis was not closed."
parsePower (MinusSign : xs) = let (factor, restTokens) = parseFactor xs in (Neg . Expression . TermBlock $ factor, restTokens)
parsePower (PlusSign : xs) = let (factor, restTokens) = parseFactor xs in (Power . Expression . TermBlock $ factor, restTokens)
parsePower _ = throw $ ParsingException "The expression provided was invalid."
