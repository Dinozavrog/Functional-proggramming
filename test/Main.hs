import Test.Hspec
import Language.Colon.Semantic.Spec qualified
import Test.Hspec.QuickCheck (modifyMaxSuccess)

main :: IO ()
main = hspec do
  modifyMaxSuccess (const 300) do
    describe "Language.Colon.Semantic" Language.Colon.Semantic.Spec.spec