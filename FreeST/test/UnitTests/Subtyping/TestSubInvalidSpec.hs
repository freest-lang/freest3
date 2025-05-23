module Subtyping.TestSubInvalidSpec (spec) where

import           Equivalence.Subtyping (subtype) 
import           Typing.Rename
import           SpecUtils

matchInvalidSpec :: [String] -> Spec
matchInvalidSpec [st, su] =
  it (show t ++ " </: " ++  show u) 
     ({-# SCC "SUB_TEST_CALL" #-} (subtype t u) `shouldBe` False)
  where
    [t, u] = renameTypes [read st, read su]

spec :: Spec
spec = do
  t <- runIO $ readFromFile "test/UnitTests/Subtyping/TestSubInvalid.txt"
  describe "Invalid Sub Test" $ mapM_ matchInvalidSpec (chunksOf 2 t)

main :: IO ()
main = hspec spec

