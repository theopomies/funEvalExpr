import ArgsSpec (argsSpec)
import E2ESpec (e2eSpec)
import EvaluatorSpec (evaluatorSpec)
import LexerSpec (lexerSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  argsSpec
  lexerSpec
  e2eSpec
  evaluatorSpec
