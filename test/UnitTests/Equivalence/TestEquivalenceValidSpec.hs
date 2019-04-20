module Equivalence.TestEquivalenceValidSpec(spec) where

import           Syntax.Kinds (Kind)
import           Syntax.Types
import           Syntax.TypeVariables
import           Syntax.Base
import           Equivalence.Equivalence
import           SpecHelper
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  -- runIO $ error $ show $ chunksOf 3 t
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 t)


matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] =
  it (k ++ "  |-  " ++ t ++ " ~ " ++  u) $
    {-# SCC "EQUIVALENT_TEST_CALL" #-}equivalent  Map.empty (Map.fromList (readKenv k)) (read t :: Type) (read u :: Type) `shouldBe` True

--  :: [(String,Kind)]
readKenv s =
  map (\(x,k) -> (mkVar defaultPos x, k)) xs
  where xs = read s :: [(String,Kind)]
  
main :: IO ()
main = hspec spec

