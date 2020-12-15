module Equivalence.TestEquivalenceValidSpec
  ( spec
  )
where

import           Equivalence.Equivalence        ( equivalent )
import           Validation.Rename              ( renameTypes )
import qualified Data.Map.Strict               as Map
import           SpecHelper

-- Note that the tests cases should be well formed (kinded)

matchValidSpec :: [String] -> Spec
matchValidSpec [t1, t2] = it
  ("|- " ++ t1 ++ " ~ " ++ t2)
  (equivalent Map.empty t1' t2' `shouldBe` True)
  where [t1', t2'] = renameTypes [read t1, read t2]

spec :: Spec
spec = do
  tests <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 2 tests)

main :: IO ()
main = hspec spec

