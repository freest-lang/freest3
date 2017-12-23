module Types.ParseSpec(spec) where

import SpecHelper
import Control.Exception (evaluate)
import Types.TestParse
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import qualified Data.Map.Strict as Map

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let treeChannelRead = "(rec TreeChannel . +{Leaf:Skip,Node:!Int;TreeChannel;TreeChannel})"
  let treeChannelType = (Rec "TreeChannel" (InternalChoice $ Map.fromList [("Leaf", Skip),("Node", (Semi (Semi (Out IntType)(Var "TreeChannel")) (Var "TreeChannel")))]))

  describe "Unit tests" $ do
    fromHUnitTest allTests
  describe "Paper examples (more complex tests)" $ do

    it "TreeChannel" $ do
      (read  treeChannelRead :: Type) `shouldBe` treeChannelType

    it "TreeChannel;alpha" $ do
        (read ("rec alpha . "++treeChannelRead++";alpha") :: Type) `shouldBe`
          (Rec "alpha" (Semi (treeChannelType) (Var "alpha")))

    it "rec alpha . !int;TreeChannel;TreeChannel;alpha" $ do
      (read ("rec alpha . !Int;" ++ treeChannelRead ++ ";" ++ treeChannelRead ++ ";alpha") :: Type) `shouldBe`
        (Rec "alpha" (Semi (Semi (Semi (Out IntType) (treeChannelType)) (treeChannelType)) (Var "alpha")))

    it "rec alpha . (!int;(TreeChannel;TreeChannel));alpha" $ do
      (read ("rec alpha . (!Int;("++treeChannelRead++";"++treeChannelRead++"));alpha") :: Type) `shouldBe`
        (Rec "alpha" (Semi (Semi (Out IntType) (Semi treeChannelType treeChannelType)) (Var "alpha")))

    it "rec alpha . !int;((TreeChannel;TreeChannel);alpha)" $ do
      (read ("rec alpha . !Int;(("++treeChannelRead++";"++treeChannelRead++");alpha)") :: Type) `shouldBe`
        (Rec "alpha" (Semi (Out IntType) (Semi (Semi treeChannelType treeChannelType) (Var "alpha"))))

    it "rec alpha . (TreeChannel;TreeChannel);alpha" $ do
      (read ("rec alpha . ("++treeChannelRead++";"++treeChannelRead++");alpha") :: Type) `shouldBe`
        (Rec "alpha" (Semi (Semi treeChannelType treeChannelType) (Var "alpha")))

    it "rec alpha . TreeChannel;(TreeChannel;alpha)" $ do
      (read ("rec alpha ."++treeChannelRead++";("++treeChannelRead++";alpha)") :: Type) `shouldBe`
        (Rec "alpha" (Semi treeChannelType (Semi  treeChannelType (Var "alpha"))))

    it "forall beta . TreeChannel -> TreeChannel; beta -> beta" $ do
      (read ("forall beta ."++treeChannelRead++"->"++treeChannelRead++";beta->beta)") :: Type) `shouldBe`
        (Forall "beta" (Semi (UnFun treeChannelType treeChannelType) (UnFun (Var "beta")(Var "beta"))))

  describe "Remote tree transformation (Listing 2)" $ do
    let xFormChanRead = "(rec xFormChan . +{Leaf:Skip,Node:!Int;xFormChan;xFormChan;?Int})"
    let xFormChanType = (Rec "xFormChan" (InternalChoice $ Map.fromList ([("Leaf",Skip),("Node",(Semi (Semi (Semi (Out IntType)(Var "xFormChan")) (Var "xFormChan"))(In IntType)))])))

    let xFormChanDualRead = "(rec xFormChan . &{Leaf:Skip,Node:?Int;xFormChan;xFormChan;!Int})"
    let xFormChanDualType = (Rec "xFormChan" (ExternalChoice $ Map.fromList ([("Leaf",Skip),("Node",(Semi (Semi (Semi (In IntType)(Var "xFormChan")) (Var "xFormChan"))(Out IntType)))])))

    it "xFormChan Type" $ do
      (read (xFormChanRead) :: Type) `shouldBe` xFormChanType

    it "xFormChan Dual Type" $ do
      (read (xFormChanDualRead) :: Type) `shouldBe` xFormChanDualType

    it "transform" $ do
      (read ("forall alpha . " ++ treeChannelRead ++ "->" ++ xFormChanRead ++ ";alpha->("++treeChannelRead++",alpha)")) `shouldBe`
        (Forall "alpha" (Semi (UnFun treeChannelType xFormChanType) (UnFun (Var "alpha") (Pair treeChannelType (Var "alpha")))))

    it "treeSum" $ do
      (read ("forall alpha . " ++ xFormChanDualRead ++ ";alpha -o (Int, alpha) ")) `shouldBe`
        (Forall "alpha" (Semi xFormChanDualType (LinFun  (Var "alpha") (Pair (Basic IntType) (Var "alpha")))))

  describe "Arithmetic expression server (Listing 3)" $ do
    let termChanRead = "(rec TermChan . +{Const:!Int,Add:TermChan;TermChan,Mult:TermChan;TermChan} )"
    let termChanType = (Rec "TermChan" (InternalChoice $ Map.fromList ([("Const",(Out IntType)),("Add",(Semi (Var "TermChan")(Var "TermChan"))),("Mult",(Semi (Var "TermChan")(Var "TermChan")))])))

    let termChanDualRead = "(rec TermChan . &{Const:?Int,Add:TermChan;TermChan,Mult:TermChan;TermChan} )"
    let termChanDualType = (Rec "TermChan" (ExternalChoice $ Map.fromList ([("Const",(In IntType)),("Add",(Semi (Var "TermChan")(Var "TermChan"))),("Mult",(Semi (Var "TermChan")(Var "TermChan")))])))

    it "TermChan Type" $ do
      (read (termChanRead) :: Type) `shouldBe` termChanType

    it "TermChan Dual Type" $ do
      (read (termChanDualRead) :: Type) `shouldBe` termChanDualType

    it "computeService" $ do
      (read (termChanDualRead ++ ";!Int->Skip") :: Type) `shouldBe`  (Semi termChanDualType (UnFun (Out IntType) Skip))

    it "receiveEval" $ do
      (read ("forall alpha ." ++ termChanDualRead ++ ";!Int->(Int, alpha)") :: Type) `shouldBe`
            (Forall "alpha" (Semi termChanDualType (UnFun (Out IntType)(Pair (Basic IntType)(Var "alpha")))))

    it "client" $ do
      (read (termChanRead ++ ";?Int->(Int,Skip)") :: Type) `shouldBe` (Semi (termChanType) (UnFun (In IntType)(Pair (Basic IntType) Skip)))

  describe "Lazy tree traversal" $ do
    it "xploreTreeChan" $ do
      pendingWith "need to make this set of tests"
      --(read () :: Type) `shouldBe` ()
