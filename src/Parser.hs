module Parser (Expression (..), Term (..), Factor (..), Power (..), parse) where

import Error (EvalExprException (ParsingException))
import GHC.Exception (throw)
import Lexer (Token (..), Tokens)

data Expression
  = Add Term Term
  | Sub Term Term
  | Expression Term

data Term
  = Mul Factor Factor
  | Div Factor Factor
  | Term Factor

data Factor
  = Factor Power
  | Pow Power Factor

data Power
  = Value Double
  | Power Expression
  | Neg Expression
  | Pos Expression

parse :: Tokens -> Expression
parse tokens
  | null (snd . parseExpression $ tokens) = fst $ parseExpression tokens
  | otherwise = throw $ ParsingException "The expression provided was invalid."

parseExpression :: Tokens -> (Expression, Tokens)
parseExpression = parseExpression' . parseTerm

parseExpression' :: (Term, Tokens) -> (Expression, Tokens)
parseExpression' (firstTerm, PlusSign : xs) = let (secondTerm, tokens) = parseTerm xs in parseExpression' (Term . Factor . Power $ Add firstTerm secondTerm, tokens)
parseExpression' (firstTerm, MinusSign : xs) = let (secondTerm, tokens) = parseTerm xs in parseExpression' (Term . Factor . Power $ Sub firstTerm secondTerm, tokens)
parseExpression' (term, tokens) = (Expression term, tokens)

parseTerm :: Tokens -> (Term, Tokens)
parseTerm = parseTerm' . parseFactor

parseTerm' :: (Factor, Tokens) -> (Term, Tokens)
parseTerm' (firstFactor, MultiplySign : xs) = let (secondFactor, tokens) = parseFactor xs in parseTerm' (Factor . Power . Expression $ Mul firstFactor secondFactor, tokens)
parseTerm' (firstFactor, DivideSign : xs) = let (secondFactor, tokens) = parseFactor xs in parseTerm' (Factor . Power . Expression $ Div firstFactor secondFactor, tokens)
parseTerm' (factor, tokens) = (Term factor, tokens)

parseFactor :: Tokens -> (Factor, Tokens)
parseFactor = parseFactor' . parsePower

parseFactor' :: (Power, Tokens) -> (Factor, Tokens)
parseFactor' (firstPower, PowerSign : xs) = let (secondPower, tokens) = parseFactor xs in parseFactor' (Power . Expression . Term $ Pow firstPower secondPower, tokens)
parseFactor' (factor, tokens) = (Factor factor, tokens)

parsePower :: Tokens -> (Power, Tokens)
parsePower ((Number n) : xs) = (Value n, xs)
parsePower (OpenParenthesis : xs)
  | null (snd . parseExpression $ xs) = throw $ ParsingException "An open parenthesis was not closed."
  | head (snd . parseExpression $ xs) == CloseParenthesis = let (expression, restTokens) = parseExpression xs in (Power expression, tail restTokens)
  | otherwise = throw $ ParsingException "An open parenthesis was not closed.2"
parsePower (MinusSign : xs) = let (power, restTokens) = parsePower xs in (Neg . Expression . Term . Factor $ power, restTokens)
parsePower (PlusSign : xs) = let (power, restTokens) = parsePower xs in (power, restTokens)
parsePower _ = throw $ ParsingException "The expression provided was invalid."
