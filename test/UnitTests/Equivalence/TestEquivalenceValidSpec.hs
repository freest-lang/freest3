module Equivalence.TestEquivalenceValidSpec(spec) where

import           Parse.Lexer (defaultPos)
import           Syntax.Bind
import           Equivalence.TypeEquivalence
import           SpecHelper
import           Syntax.Kinds (Kind)
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Equivalence/TestEquivalenceValid.txt"
  -- runIO $ error $ show $ chunksOf 3 t
  describe "Valid Equivalence Test" $ mapM_ matchValidSpec (chunksOf 3 t)


matchValidSpec :: [String] -> Spec
matchValidSpec [k, t, u] =
  it (k ++ "  |-  " ++ t ++ " ~ " ++  u) $
    {-# SCC "EQUIVALENT_TEST_CALL" #-}equivalent (Map.fromList (readKenv k)) (read t) (read u) `shouldBe` True

--  :: [(String,Kind)]
readKenv s =
  map (\(x,k) -> (TBind defaultPos x, k)) xs
  where xs = read s :: [(String,Kind)]
  
main :: IO ()
main = hspec spec

