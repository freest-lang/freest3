import           Test.Hspec
import qualified Equivalence.TestEquivalenceValidSpec as Equiv
import qualified Equivalence.TestEquivalenceInvalidSpec as InvEquiv

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Valid tests"  Equiv.spec
  describe "Invalid tests"  InvEquiv.spec
