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
    describe "SubKind reflax" $ do
      it "*S <: *S" $ us p <: us p `shouldBe` True 
      it "1S <: 1S" $ ls p <: ls p `shouldBe` True 
      it "*T <: *T" $ ut p <: ut p `shouldBe` True 
      it "1T <: 1T" $ lt p <: lt p `shouldBe` True 
      it "*E <: *E" $ ue p <: ul p `shouldBe` True 
      it "1E <: 1E" $ la p <: la p `shouldBe` True
    describe "SubKind relation" $ do  
      it "*S <: *T" $ us p <: ut p `shouldBe` True
      it "*S <: 1S" $ us p <: ls p `shouldBe` True 
      it "1S <: 1T" $ ls p <: lt p `shouldBe` True 
      it "*T <: 1T" $ ut p <: lt p `shouldBe` True 
      it "*S <: 1T" $ us p <: lt p `shouldBe` True
      it "*E <: 1E" $ ul p <: la p `shouldBe` True
      it "*E <: *S" $ ul p <: us p `shouldBe` True
      it "*E <: 1S" $ ul p <: ls p `shouldBe` True
      it "*E <: *T" $ ul p <: ut p `shouldBe` True
      it "*E <: 1T" $ ul p <: lt p `shouldBe` True    
      it "1E <: 1S" $ ul p <: ls p `shouldBe` True
      it "1E <: 1T" $ ul p <: lt p `shouldBe` True


(⊔) :: Join t => t -> t -> t
(⊔) = join

testJoin :: SpecWith ()
testJoin =
  describe "Join" $ do
   describe "Join reflax" $ do
     it "1T ⊔ 1T" $ lt p ⊔ lt p `shouldBe` lt p   
     it "*T ⊔ *T" $ ut p ⊔ ut p `shouldBe` ut p 
     it "1S ⊔ 1S" $ ls p ⊔ ls p `shouldBe` ls p 
     it "*S ⊔ *S" $ us p ⊔ us p `shouldBe` us p
     it "*E ⊔ *E" $ ul p ⊔ ul p `shouldBe` ul p
     it "1E ⊔ 1E" $ la p ⊔ la p `shouldBe` la p
   describe "Join relation" $ do
     it "1T ⊔ 1S" $ lt p ⊔ ls p `shouldBe` lt p 
     it "1T ⊔ *T" $ lt p ⊔ ut p `shouldBe` lt p
     it "1T ⊔ *S" $ lt p ⊔ us p `shouldBe` lt p 
     it "1S ⊔ *T" $ ls p ⊔ ut p `shouldBe` lt p 
     it "1S ⊔ *S" $ ls p ⊔ us p `shouldBe` ls p 
     it "*T ⊔ *S" $ ut p ⊔ us p `shouldBe` ut p   
     it "1T ⊔ 1E" $ lt p ⊔ la p `shouldBe` lt p
     it "1S ⊔ 1E" $ ls p ⊔ la p `shouldBe` ls p
     it "*T ⊔ 1E" $ ut p ⊔ la p `shouldBe` lt p
     it "*S ⊔ 1E" $ us p ⊔ la p `shouldBe` ls p
     it "1T ⊔ *E" $ lt p ⊔ ul p `shouldBe` lt p
     it "1S ⊔ *E" $ ls p ⊔ ul p `shouldBe` ls p
     it "*T ⊔ *E" $ ut p ⊔ ul p `shouldBe` ut p
     it "*S ⊔ *E" $ us p ⊔ ul p `shouldBe` us p
   describe "Join relation reversed args" $ do
     it "1S ⊔ 1T" $ ls p ⊔ lt p `shouldBe` lt p 
     it "*T ⊔ 1T" $ ut p ⊔ lt p `shouldBe` lt p
     it "*S ⊔ 1T" $ us p ⊔ lt p `shouldBe` lt p 
     it "*T ⊔ 1S" $ ut p ⊔ ls p `shouldBe` lt p 
     it "*S ⊔ 1S" $ us p ⊔ ls p `shouldBe` ls p 
     it "*S ⊔ *T" $ us p ⊔ ut p `shouldBe` ut p
     it "1E ⊔ 1T" $ la p ⊔ lt p `shouldBe` lt p
     it "1E ⊔ 1S" $ la p ⊔ ls p `shouldBe` ls p
     it "1E ⊔ *T" $ la p ⊔ ut p `shouldBe` lt p
     it "1E ⊔ *S" $ la p ⊔ us p `shouldBe` ls p
     it "*E ⊔ 1T" $ ul p ⊔ lt p `shouldBe` lt p
     it "*E ⊔ 1S" $ ul p ⊔ ls p `shouldBe` ls p
     it "*E ⊔ *T" $ ul p ⊔ ut p `shouldBe` ut p
     it "*E ⊔ *S" $ ul p ⊔ us p `shouldBe` us p
    
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
      it "*E ⊓ *E" $ ul p ⊓ ul p `shouldBe` ul p
      it "1E ⊓ 1E" $ la p ⊓ la p `shouldBe` la p
    describe "Meet relation" $ do
      it "1T ⊓ 1S" $ lt p ⊓ ls p `shouldBe` ls p 
      it "1T ⊓ *T" $ lt p ⊓ ut p `shouldBe` ut p
      it "1T ⊓ *S" $ lt p ⊓ us p `shouldBe` us p 
      it "1S ⊓ *T" $ ls p ⊓ ut p `shouldBe` us p 
      it "1S ⊓ *S" $ ls p ⊓ us p `shouldBe` us p 
      it "*T ⊓ *S" $ ut p ⊓ us p `shouldBe` us p
      it "1T ⊓ 1E" $ lt p ⊓ la p `shouldBe` la p
      it "1S ⊓ 1E" $ ls p ⊓ la p `shouldBe` la p
      it "*T ⊓ 1E" $ ut p ⊓ la p `shouldBe` ul p
      it "*S ⊓ 1E" $ us p ⊓ la p `shouldBe` ul p
      it "1T ⊓ *E" $ lt p ⊓ ul p `shouldBe` ul p
      it "1S ⊓ *E" $ ls p ⊓ ul p `shouldBe` ul p
      it "*T ⊓ *E" $ ut p ⊓ ul p `shouldBe` ul p
      it "*S ⊓ *E" $ us p ⊓ ul p `shouldBe` ul p      
    describe "Meet relation reversed args" $ do
      it "1S ⊓ 1T" $ ls p ⊓ lt p `shouldBe` ls p 
      it "*T ⊓ 1T" $ ut p ⊓ lt p `shouldBe` ut p
      it "*S ⊓ 1T" $ us p ⊓ lt p `shouldBe` us p 
      it "*T ⊓ 1S" $ ut p ⊓ ls p `shouldBe` us p 
      it "*S ⊓ 1S" $ us p ⊓ ls p `shouldBe` us p 
      it "*S ⊓ *T" $ us p ⊓ ut p `shouldBe` us p      
      it "1E ⊓ 1T" $ la p ⊓ lt p `shouldBe` la p
      it "1E ⊓ 1S" $ la p ⊓ ls p `shouldBe` la p
      it "1E ⊓ *T" $ la p ⊓ ut p `shouldBe` ul p
      it "1E ⊓ *S" $ la p ⊓ us p `shouldBe` ul p
      it "*E ⊓ 1T" $ ul p ⊓ lt p `shouldBe` ul p
      it "*E ⊓ 1S" $ ul p ⊓ ls p `shouldBe` ul p
      it "*E ⊓ *T" $ ul p ⊓ ut p `shouldBe` ul p
      it "*E ⊓ *S" $ ul p ⊓ us p `shouldBe` ul p
