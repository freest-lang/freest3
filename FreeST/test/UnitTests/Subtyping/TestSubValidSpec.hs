{-# LANGUAGE BlockArguments #-}

module Subtyping.TestSubValidSpec (spec) where

import           Typing.Rename
import           Equivalence.Subtyping
import           SpecUtils

matchValidSpec :: [String] -> Spec
matchValidSpec [st, su] =
  it (show t ++ " <: " ++  show u) 
     ({-# SCC "SUB_TEST_CALL" #-} (subtype t u) `shouldBe` True)
  where 
    [t, u] = renameTypes [read st, read su]

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Subtyping/TestSubValid.txt"
  describe "Valid Subtyping Test" $ mapM_ matchValidSpec (chunksOf 2 t)

main :: IO ()
main = hspec spec
