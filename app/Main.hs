module Main where

import Display (display)
import Evaluator (evaluate)
import Lexer (tokenize)
import Parser (parse)

main :: IO ()
main = display . evaluate . parse . tokenize $ "1+1"
