module Types.TestTypesSpec where

import SpecHelper
import qualified Data.Set as Set

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Testing equality" $ do
    it "Skip == Skip" $ do
      Skip == Skip `shouldBe` True

    it "(Var x) == (Var y)" $ do
      (Var "x") == (Var "y") `shouldBe` False

    it "(Forall a (Var a)) == (Forall b (Var b))" $ do
      (Forall "a" (Var "a")) == (Forall "b" (Var "b")) `shouldBe` True

    it "(Forall b (Var b)) == (Forall b (Var b))" $ do
      (Forall "b" (Var "b")) == (Forall "b" (Var "b")) `shouldBe` True

    --TODO: check if it is supposed to work like this
    it "(Forall a (Var b)) == (Forall b (Var b))" $ do
      (Forall "a" (Var "b")) == (Forall "b" (Var "b")) `shouldBe` True

    it "(Forall a (Var b)) == (Forall b (Var b))" $ do
      (Forall "c" (Var "a")) == (Forall "b" (Var "b")) `shouldBe` False

    it "(Forall a (Forall b (Var a))) == (Forall c (Forall d (Var c)))" $ do
      (Forall "a" (Forall "b" (Var "a"))) == (Forall "c" (Forall "d" (Var "c"))) `shouldBe` True


-- two vars with the same name ie: Forall "a" (Forall "a" (Var "a"))
-- negative tests
--  evaluate (read "" :: Type) `shouldThrow` anyException
