module Error where

import Control.Exception (Exception)
import System.Exit (ExitCode (ExitFailure), exitWith)

data EvalExprException
  = ParsingException String
  | LexingException String
  | ComputingException String
  deriving (Show)

instance Exception EvalExprException

exceptionHandler :: EvalExprException -> IO ()
exceptionHandler (ParsingException text) = putStrLn text >> exitWith (ExitFailure 84)
exceptionHandler (LexingException text) = putStrLn text >> exitWith (ExitFailure 84)
exceptionHandler (ComputingException text) = putStrLn text >> exitWith (ExitFailure 84)
