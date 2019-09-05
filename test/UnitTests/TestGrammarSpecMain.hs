import           Test.Hspec
import qualified Equivalence.TestGrammarValidSpec as BisimValid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Bisimilar grammars valid tests"  BisimValid.spec
