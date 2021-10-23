import ArgsSpec (argsSpec)
import LexerSpec (lexerSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  argsSpec
  lexerSpec
