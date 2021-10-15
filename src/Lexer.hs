module Lexer (Token (..), Tokens, tokenize) where

import Args (ExpressionString (ExpressionString))
import Control.Exception.Base (throw)
import Data.Char (isDigit)
import Error (EvalExprException (LexingException))
import Text.Read (readMaybe)

data Token
  = PlusSign
  | MinusSign
  | MultiplySign
  | DivideSign
  | PowerSign
  | OpenParenthesis
  | CloseParenthesis
  | Number Double
  deriving (Eq)

type Tokens = [Token]

tokenize :: ExpressionString -> Tokens
tokenize (ExpressionString []) = []
tokenize (ExpressionString ('+' : rest)) = PlusSign : tokenize (ExpressionString rest)
tokenize (ExpressionString ('-' : rest)) = MinusSign : tokenize (ExpressionString rest)
tokenize (ExpressionString ('*' : rest)) = MultiplySign : tokenize (ExpressionString rest)
tokenize (ExpressionString ('/' : rest)) = DivideSign : tokenize (ExpressionString rest)
tokenize (ExpressionString ('^' : rest)) = PowerSign : tokenize (ExpressionString rest)
tokenize (ExpressionString ('(' : rest)) = OpenParenthesis : tokenize (ExpressionString rest)
tokenize (ExpressionString (')' : rest)) = CloseParenthesis : tokenize (ExpressionString rest)
tokenize (ExpressionString (' ' : rest)) = tokenize (ExpressionString rest)
tokenize (ExpressionString str) = tokenizeNumber (fst . extractMaybeDouble $ str) : tokenize (ExpressionString (snd . extractMaybeDouble $ str))

tokenizeNumber :: Maybe Double -> Token
tokenizeNumber (Just value) = Number value
tokenizeNumber _ = throw $ LexingException "A value entered was not a valid token."

extractMaybeDouble :: String -> (Maybe Double, String)
extractMaybeDouble str = let (numberStr, rest) = extractMaybeDoubleAsString str in (readMaybe numberStr, rest)

extractMaybeDoubleAsString :: String -> (String, String)
extractMaybeDoubleAsString ('.' : rest) = let (nbrs, end) = extractMaybeDoubleAsString rest in ('.' : nbrs, end)
extractMaybeDoubleAsString line@(char : rest)
  | isDigit char = let (nbrs, end) = extractMaybeDoubleAsString rest in (char : nbrs, end)
  | otherwise = ([], line)
extractMaybeDoubleAsString _ = ([], [])
