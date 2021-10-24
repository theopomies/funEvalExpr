module Error (exceptionHandler, EvalExprException (..)) where

import Control.Exception (Exception)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

data EvalExprException
  = ParsingException String
  | LexingException String
  | ComputingException String
  | InvalidArgsException
  | HelpException
  deriving (Show, Eq)

instance Exception EvalExprException

exceptionHandler :: EvalExprException -> IO ()
exceptionHandler (ParsingException   text) = putStrLn text        >> exitWith (ExitFailure 84)
exceptionHandler (LexingException    text) = putStrLn text        >> exitWith (ExitFailure 84)
exceptionHandler (ComputingException text) = putStrLn text        >> exitWith (ExitFailure 84)
exceptionHandler InvalidArgsException      = putStrLn helpMessage >> exitWith (ExitFailure 84)
exceptionHandler HelpException             = putStrLn helpMessage >> exitSuccess

helpMessage :: String
helpMessage =
  "USAGE:\n\
  \\t./funEvalExpr expression\n\
  \DESCRIPTION:\n\
  \\tEvaluates the expression passed in parameter.\n\
  \PARAMS:\n\
  \\texpression: The expression to evaluate (pretty obvious)."
