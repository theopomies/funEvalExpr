module Main (main) where

import Args (parseArgs)
import Control.Exception.Base (catch)
import Display (display)
import Error (exceptionHandler)
import Evaluator (evaluate)
import Lexer (tokenize)
import Parser (parse)
import System.Environment.Blank (getArgs)

main :: IO ()
main = (getArgs >>= display . evaluate . parse . tokenize . parseArgs) `catch` exceptionHandler
