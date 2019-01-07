import Test.Hspec
import qualified Validation.TestEquivalenceValidSpec as Equiv

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Validation"  Equiv.spec

