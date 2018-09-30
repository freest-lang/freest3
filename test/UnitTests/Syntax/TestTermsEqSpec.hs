module Syntax.TestTermsEqSpec(spec) where

import           SpecHelper
import           Syntax.Terms
import           Syntax.Types -- TODO REMOVE
import           Syntax.Kinds
import qualified Data.Map.Strict as Map

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do  
-- ("Node", (Semi (Message Out IntType) (Semi(Var "TreeChannel") (Var "TreeChannel"))))]))

  describe "Eq Expressions" $ do
    it "Unit" $ do
     (Unit (0,0)) == (Unit (0,0)) `shouldBe` True
    it "Int" $ do
     (Integer (0,0) 2) == (Integer (0,0) 2) `shouldBe` True
    it "Character" $ do
      (Character (0,0) 'c') == (Character (0,0) 'c') `shouldBe` True
    it "Bool" $ do
      (Boolean (0,0) True) == (Boolean (0,0) True) `shouldBe` True
    it "Bool" $ do
      (Boolean (0,0) False) == (Boolean (0,0) False) `shouldBe` True
    it "Variable" $ do
      (Variable (0,0) "x") == (Variable (0,0) "x") `shouldBe` True
    it "UnLet" $ do
      (UnLet (0,0) "x" (Integer (0,0) 2) (Integer (0,0) 2)) ==
        (UnLet (0,0) "x" (Integer (0,0) 2) (Integer (0,0) 2)) `shouldBe` True      
    it "App" $ do
      (App (0,0) (Variable (0,0) "x") (Integer (0,0) 2)) ==
        (App (0,0) (Variable (0,0) "x") (Integer (0,0) 2)) `shouldBe` True
    it "TypeApp" $ do
      (TypeApp (0,0) (Variable (0,0) "x") [(Basic IntType)]) ==
        (TypeApp (0,0) (Variable (0,0) "x") [(Basic IntType)]) `shouldBe` True
    it "Conditional" $ do
      (Conditional (0,0) (Boolean (0,0) True) (Integer (0,0) 2) (Integer (0,0) 2)) ==
        (Conditional (0,0) (Boolean (0,0) True) (Integer (0,0) 2) (Integer (0,0) 2))
           `shouldBe` True
    it "Pair" $ do
      (Pair (0,0) (Variable (0,0) "x") (Integer (0,0) 2)) ==
        (Pair (0,0) (Variable (0,0) "x") (Integer (0,0) 2)) `shouldBe` True
    it "BinLet" $ do
        (BinLet (0,0) "x" "y" (Integer (0,0) 2) (Integer (0,0) 2)) ==
          (BinLet (0,0) "x" "y" (Integer (0,0) 2) (Integer (0,0) 2)) `shouldBe` True    
    it "New" $ do
      (New (0,0) (Basic IntType)) ==
        (New (0,0) (Basic IntType)) `shouldBe` True
    it "Send" $ do
      (Send (0,0) (Variable (0,0) "x") (Variable (0,0) "c")) ==
        (Send (0,0) (Variable (0,0) "x") (Variable (0,0) "c")) `shouldBe` True
    it "Receive" $ do
      (Receive (0,0) (Variable (0,0) "c")) ==
        (Receive (0,0) (Variable (0,0) "c")) `shouldBe` True
    it "Select" $ do
      (Select (0,0) "x" (Variable (0,0) "x")) ==
        (Select (0,0) "x" (Variable (0,0) "x")) `shouldBe` True
    it "Match" $ do
      (Match (0,0) (Variable (0,0) "x") (Map.singleton "C" ("w", (Integer (0,0) 2)))) ==
        (Match (0,0) (Variable (0,0) "x") (Map.singleton "C" ("w", (Integer (0,0) 2))))
          `shouldBe` True
    it "Fork" $ do
      (Fork (0,0) (Variable (0,0) "x")) ==
        (Fork (0,0) (Variable (0,0) "x")) `shouldBe` True
    it "Constructor" $ do
      (Constructor (0,0) "x") ==
        (Constructor (0,0) "x") `shouldBe` True
    it "Case" $ do
      (Case (0,0) (Variable (0,0) "x") (Map.singleton "C" (["w"], (Integer (0,0) 2)))) ==
        (Case (0,0) (Variable (0,0) "x") (Map.singleton "C" (["w"], (Integer (0,0) 2))))
          `shouldBe` True  

    it "Ord prekinds" $ do
      Session `compare` Functional `shouldBe` LT
    it "Ord prekinds" $ do
      Functional `compare` Session `shouldBe` GT 
    it "Ord prekinds" $ do
      Session `compare` Session `shouldBe` EQ
    it "Ord prekinds" $ do
      Functional `compare` Functional `shouldBe` EQ  
 
    -- it "multiplicity" $ do
    --   Un `compare` Lin `shouldBe` LT
    -- it "multiplicity" $ do
    --   Lin `compare` Un `shouldBe` GT
    -- it "multiplicity" $ do
    --   Un `compare` Un `shouldBe` EQ
    -- it "multiplicity" $ do
    --   Lin `compare` Lin `shouldBe` EQ
 
