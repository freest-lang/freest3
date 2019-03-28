module Syntax.TestExpsEqSpec(spec) where

import           Parse.Lexer (defaultPos)
import           SpecHelper
import           Syntax.Exps
import           Syntax.Types -- TODO REMOVE
import           Syntax.Kinds
import           Syntax.Bind
import qualified Data.Map.Strict as Map

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do  
-- ("Node", (Semi (Message Out IntType) (Semi(Var "TreeChannel") (Var "TreeChannel"))))]))
  let p = defaultPos
  describe "Eq Expressions" $ do
    it "Unit" $ do
     (Unit p) == (Unit p) `shouldBe` True
    it "Int" $ do
     (Integer p 2) == (Integer p 2) `shouldBe` True
    it "Character" $ do
      (Character p 'c') == (Character p 'c') `shouldBe` True
    it "Bool" $ do
      (Boolean p True) == (Boolean p True) `shouldBe` True
    it "Bool" $ do
      (Boolean p False) == (Boolean p False) `shouldBe` True
    it "Variable" $ do
      (Variable p "x") == (Variable p "x") `shouldBe` True
    it "UnLet" $ do
      (UnLet p (Bind p "x") (Integer p 2) (Integer p 2)) ==
        (UnLet p (Bind p "x") (Integer p 2) (Integer p 2)) `shouldBe` True      
    it "App" $ do
      (App p (Variable p "x") (Integer p 2)) ==
        (App p (Variable p "x") (Integer p 2)) `shouldBe` True
    it "TypeApp" $ do
      (TypeApp p "x" [(Basic p IntType)]) ==
        (TypeApp p "x" [(Basic p IntType)]) `shouldBe` True
    it "Conditional" $ do
      (Conditional p (Boolean p True) (Integer p 2) (Integer p 2)) ==
        (Conditional p (Boolean p True) (Integer p 2) (Integer p 2))
           `shouldBe` True
    it "Pair" $ do
      (Pair p (Variable p "x") (Integer p 2)) ==
        (Pair p (Variable p "x") (Integer p 2)) `shouldBe` True
    it "BinLet" $ do
        (BinLet p (Bind p "x") (Bind p "y") (Integer p 2) (Integer p 2)) ==
          (BinLet p (Bind p "x") (Bind p "y") (Integer p 2) (Integer p 2)) `shouldBe` True    
    it "New" $ do
      (New p (Basic p IntType)) ==
        (New p (Basic p IntType)) `shouldBe` True
    it "Send" $ do
      (Send p (Variable p "x")) ==
        (Send p (Variable p "x")) `shouldBe` True
    it "Receive" $ do
      (Receive p (Variable p "c")) ==
        (Receive p (Variable p "c")) `shouldBe` True
    it "Select" $ do
      (Select p "x" (Variable p "x")) ==
        (Select p "x" (Variable p "x")) `shouldBe` True
    it "Match" $ do
      (Match p (Variable p "x") (Map.singleton (Bind p "C") (Bind p "w", (Integer p 2)))) ==
        (Match p (Variable p "x") (Map.singleton (Bind p "C") (Bind p "w", (Integer p 2))))
          `shouldBe` True
    it "Fork" $ do
      (Fork p (Variable p "x")) ==
        (Fork p (Variable p "x")) `shouldBe` True
    -- it "Constructor" $ do
    --   (Constructor p "x") ==
    --     (Constructor p "x") `shouldBe` True
    it "Case" $ do
      (Case p (Variable p "x") (Map.singleton (Bind p "C") ([Bind p "w"], (Integer p 2)))) ==
        (Case p (Variable p "x") (Map.singleton (Bind p "C") ([Bind p "w"], (Integer p 2))))
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
 

