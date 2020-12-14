module Equivalence.TestEquivalenceValidSpec
  ( spec
  )
where

import           Equivalence.Equivalence        ( equivalent )
import           Validation.Rename              ( renameTypes )
-- import qualified Data.Map.Strict               as Map
import           SpecHelper

-- Note that the tests cases should be kinded!

matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] = it
  (k ++ "  |-  " ++ t ++ " ~ " ++ u)
  (equivalent (readKenv k) t' u' `shouldBe` True)
  where [t', u'] = renameTypes [read t, read u]

spec :: Spec
spec = do
  t <- runIO
    $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 t)

main :: IO ()
main = hspec spec

