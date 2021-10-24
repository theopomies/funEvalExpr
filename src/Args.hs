module Args (parseArgs, ExpressionString (ExpressionString)) where

import Error (EvalExprException (HelpException, InvalidArgsException))
import GHC.Exception (throw)

newtype ExpressionString = ExpressionString String

parseArgs :: [String] -> ExpressionString
parseArgs ("-h"     : _) = throw HelpException
parseArgs ("--help" : _) = throw HelpException
parseArgs [expression]   = ExpressionString expression
parseArgs _              = throw InvalidArgsException