module TestTypesSpecMain where

import           Test.Hspec
import qualified Validation.TestTypesValidSpec as Types
import qualified Validation.TestTypesInvalidSpec as InvTypes

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Valid types"  Types.spec
  describe "Invalid types"  InvTypes.spec
