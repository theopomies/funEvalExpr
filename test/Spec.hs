import E2ESpec (e2eSpec)
import LexerSpec (lexerSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  lexerSpec
  e2eSpec