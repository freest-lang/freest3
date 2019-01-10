import Test.Hspec
import qualified Validation.TestEquivalenceValidSpec as Equiv
import qualified Validation.TestEquivalenceInvalidSpec as InvEquiv

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Validation"  Equiv.spec
  describe "Invalid"  InvEquiv.spec
