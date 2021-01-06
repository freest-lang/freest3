module Main.TestBisimSpecMain where

import           Test.Hspec
import qualified Equivalence.TestBisimValidSpec as BisimValid
import qualified Equivalence.TestBisimInvalidSpec as BisimInvalid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Bissim valid tests"  BisimValid.spec
--  describe "Bissim invalid tests"  BisimInvalid.spec

