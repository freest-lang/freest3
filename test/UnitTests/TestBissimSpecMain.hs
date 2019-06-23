import           Test.Hspec
import qualified Equivalence.TestBissimValidSpec as BissimValid
import qualified Equivalence.TestBissimInvalidSpec as BissimInvalid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Bissim valid tests"  BissimValid.spec
  describe "Bissim invalid tests"  BissimInvalid.spec
