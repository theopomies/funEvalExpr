import Test.Hspec ( hspec )
import LexerSpec ( lexerSpec )

main :: IO ()
main = hspec $ do
    lexerSpec
