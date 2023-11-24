module Validation.TestSubKindValidSpec ( spec ) where

import           Test.Hspec
import Syntax.Base
import Syntax.Kind
import Validation.Subkind
import Parse.Unparser

spec :: Spec
spec =
  describe "Subkind, Join and Meet" $ do
    testSubkind
    testJoin
    testMeet

p :: Span
p = defaultSpan

testSubkind :: SpecWith ()
testSubkind = 
  describe "SubKind" $ do
    describe "SubKind reflex" $ do
      it "*S <: *S" $ us p <: us p `shouldBe` True 
      it "1S <: 1S" $ ls p <: ls p `shouldBe` True 
      it "*T <: *T" $ ut p <: ut p `shouldBe` True 
      it "1T <: 1T" $ lt p <: lt p `shouldBe` True 
      it "*A <: *A" $ ua p <: ua p `shouldBe` True 
      it "1A <: 1A" $ la p <: la p `shouldBe` True
    describe "SubKind relation" $ do  
      it "*S <: *T" $ us p <: ut p `shouldBe` True
      it "*S <: 1S" $ us p <: ls p `shouldBe` True 
      it "1S <: 1T" $ ls p <: lt p `shouldBe` True 
      it "*T <: 1T" $ ut p <: lt p `shouldBe` True 
      it "*S <: 1T" $ us p <: lt p `shouldBe` True
      it "*A <: 1A" $ ua p <: la p `shouldBe` True
      it "*A <: *S" $ ua p <: us p `shouldBe` True
      it "*A <: 1S" $ ua p <: ls p `shouldBe` True
      it "*A <: *T" $ ua p <: ut p `shouldBe` True
      it "*A <: 1T" $ ua p <: lt p `shouldBe` True    
      it "1A <: 1S" $ la p <: ls p `shouldBe` True
      it "1A <: 1T" $ la p <: lt p `shouldBe` True


(⊔) :: Join t => t -> t -> t
(⊔) = join


testJoin :: SpecWith ()
testJoin =
  describe "Join" $ do
   describe "Join reflex" $ do
     it "1T ⊔ 1T" $ lt p ⊔ lt p `shouldBe` lt p   
     it "*T ⊔ *T" $ ut p ⊔ ut p `shouldBe` ut p 
     it "1S ⊔ 1S" $ ls p ⊔ ls p `shouldBe` ls p 
     it "*S ⊔ *S" $ us p ⊔ us p `shouldBe` us p
     it "*A ⊔ *A" $ ua p ⊔ ua p `shouldBe` ua p
     it "1A ⊔ 1A" $ la p ⊔ la p `shouldBe` la p
   describe "Join relation" $ do
     it "1T ⊔ 1S" $ lt p ⊔ ls p `shouldBe` lt p 
     it "1T ⊔ *T" $ lt p ⊔ ut p `shouldBe` lt p
     it "1T ⊔ *S" $ lt p ⊔ us p `shouldBe` lt p 
     it "1S ⊔ *T" $ ls p ⊔ ut p `shouldBe` lt p 
     it "1S ⊔ *S" $ ls p ⊔ us p `shouldBe` ls p 
     it "*T ⊔ *S" $ ut p ⊔ us p `shouldBe` ut p   
     it "1T ⊔ 1A" $ lt p ⊔ la p `shouldBe` lt p
     it "1S ⊔ 1A" $ ls p ⊔ la p `shouldBe` ls p
     it "*T ⊔ 1A" $ ut p ⊔ la p `shouldBe` lt p
     it "*S ⊔ 1A" $ us p ⊔ la p `shouldBe` ls p
     it "1T ⊔ *A" $ lt p ⊔ ua p `shouldBe` lt p
     it "1S ⊔ *A" $ ls p ⊔ ua p `shouldBe` ls p
     it "*T ⊔ *A" $ ut p ⊔ ua p `shouldBe` ut p
     it "*S ⊔ *A" $ us p ⊔ ua p `shouldBe` us p
   describe "Join relation reversed args" $ do
     it "1S ⊔ 1T" $ ls p ⊔ lt p `shouldBe` lt p 
     it "*T ⊔ 1T" $ ut p ⊔ lt p `shouldBe` lt p
     it "*S ⊔ 1T" $ us p ⊔ lt p `shouldBe` lt p 
     it "*T ⊔ 1S" $ ut p ⊔ ls p `shouldBe` lt p 
     it "*S ⊔ 1S" $ us p ⊔ ls p `shouldBe` ls p 
     it "*S ⊔ *T" $ us p ⊔ ut p `shouldBe` ut p
     it "1A ⊔ 1T" $ la p ⊔ lt p `shouldBe` lt p
     it "1A ⊔ 1S" $ la p ⊔ ls p `shouldBe` ls p
     it "1A ⊔ *T" $ la p ⊔ ut p `shouldBe` lt p
     it "1A ⊔ *S" $ la p ⊔ us p `shouldBe` ls p
     it "*A ⊔ 1T" $ ua p ⊔ lt p `shouldBe` lt p
     it "*A ⊔ 1S" $ ua p ⊔ ls p `shouldBe` ls p
     it "*A ⊔ *T" $ ua p ⊔ ut p `shouldBe` ut p
     it "*A ⊔ *S" $ ua p ⊔ us p `shouldBe` us p
    
(⊓) :: Meet t => t -> t -> t
(⊓) = meet

testMeet :: SpecWith ()
testMeet =  
  describe "Meet" $ do
    describe "Meet reflax" $ do
      it "1T ⊓ 1T" $ lt p ⊓ lt p `shouldBe` lt p 
      it "*T ⊓ *T" $ ut p ⊓ ut p `shouldBe` ut p 
      it "1S ⊓ 1S" $ ls p ⊓ ls p `shouldBe` ls p 
      it "*S ⊓ *S" $ us p ⊓ us p `shouldBe` us p
      it "*A ⊓ *A" $ ua p ⊓ ua p `shouldBe` ua p
      it "1A ⊓ 1A" $ la p ⊓ la p `shouldBe` la p
    describe "Meet relation" $ do
      it "1T ⊓ 1S" $ lt p ⊓ ls p `shouldBe` ls p 
      it "1T ⊓ *T" $ lt p ⊓ ut p `shouldBe` ut p
      it "1T ⊓ *S" $ lt p ⊓ us p `shouldBe` us p 
      it "1S ⊓ *T" $ ls p ⊓ ut p `shouldBe` us p 
      it "1S ⊓ *S" $ ls p ⊓ us p `shouldBe` us p 
      it "*T ⊓ *S" $ ut p ⊓ us p `shouldBe` us p
      it "1T ⊓ 1A" $ lt p ⊓ la p `shouldBe` la p
      it "1S ⊓ 1A" $ ls p ⊓ la p `shouldBe` la p
      it "*T ⊓ 1A" $ ut p ⊓ la p `shouldBe` ua p
      it "*S ⊓ 1A" $ us p ⊓ la p `shouldBe` ua p
      it "1T ⊓ *A" $ lt p ⊓ ua p `shouldBe` ua p
      it "1S ⊓ *A" $ ls p ⊓ ua p `shouldBe` ua p
      it "*T ⊓ *A" $ ut p ⊓ ua p `shouldBe` ua p
      it "*S ⊓ *A" $ us p ⊓ ua p `shouldBe` ua p      
    describe "Meet relation reversed args" $ do
      it "1S ⊓ 1T" $ ls p ⊓ lt p `shouldBe` ls p 
      it "*T ⊓ 1T" $ ut p ⊓ lt p `shouldBe` ut p
      it "*S ⊓ 1T" $ us p ⊓ lt p `shouldBe` us p 
      it "*T ⊓ 1S" $ ut p ⊓ ls p `shouldBe` us p 
      it "*S ⊓ 1S" $ us p ⊓ ls p `shouldBe` us p 
      it "*S ⊓ *T" $ us p ⊓ ut p `shouldBe` us p      
      it "1A ⊓ 1T" $ la p ⊓ lt p `shouldBe` la p
      it "1A ⊓ 1S" $ la p ⊓ ls p `shouldBe` la p
      it "1A ⊓ *T" $ la p ⊓ ut p `shouldBe` ua p
      it "1A ⊓ *S" $ la p ⊓ us p `shouldBe` ua p
      it "*A ⊓ 1T" $ ua p ⊓ lt p `shouldBe` ua p
      it "*A ⊓ 1S" $ ua p ⊓ ls p `shouldBe` ua p
      it "*A ⊓ *T" $ ua p ⊓ ut p `shouldBe` ua p
      it "*A ⊓ *S" $ ua p ⊓ us p `shouldBe` ua p
