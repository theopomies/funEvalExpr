module Lexer (Token (..), tokenize) where

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

type Tokens = [Token]

tokenize :: String -> Tokens
tokenize [] = []
tokenize ('+' : rest) = PlusSign : tokenize rest
tokenize ('-' : rest) = MinusSign : tokenize rest
tokenize ('*' : rest) = MultiplySign : tokenize rest
tokenize ('/' : rest) = DivideSign : tokenize rest
tokenize ('^' : rest) = PowerSign : tokenize rest
tokenize ('(' : rest) = OpenParenthesis : tokenize rest
tokenize (')' : rest) = CloseParenthesis : tokenize rest
tokenize (' ' : rest) = tokenize rest
tokenize str = tokenizeNumber (fst . extractMaybeDouble $ str) : tokenize (snd . extractMaybeDouble $ str)

tokenizeNumber :: Maybe Double -> Token
tokenizeNumber (Just value) = Number value
tokenizeNumber _ = throw $ LexingException "A value entered was not a valid double."

extractMaybeDouble :: String -> (Maybe Double, String)
extractMaybeDouble str = let (numberStr, rest) = extractMaybeDoubleAsString str in (readMaybe numberStr, rest)

extractMaybeDoubleAsString :: String -> (String, String)
extractMaybeDoubleAsString ('.' : rest) = let (nbrs, end) = extractMaybeDoubleAsString rest in ('.' : nbrs, end)
extractMaybeDoubleAsString line@(char : rest)
  | isDigit char = let (nbrs, end) = extractMaybeDoubleAsString rest in (char : nbrs, end)
  | otherwise = ([], line)
extractMaybeDoubleAsString _ = ([], [])
