import ArgsSpec (argsSpec)
import E2ESpec (e2eSpec)
import LexerSpec (lexerSpec)
import EvaluatorSpec (evaluatorSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  argsSpec
  lexerSpec
  e2eSpec
  evaluatorSpec
