module Types.TestParserValidSpec(spec) where

import SpecHelper
import Control.Exception (evaluate)
--import Types.TestParseUnit
--import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import qualified Data.Map.Strict as Map
import Test.Hspec.Expectations (anyException, shouldThrow)

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let treeChannelRead = "(rec TreeChannel . +{Leaf:Skip,Node:!Int;TreeChannel;TreeChannel})"
  let treeChannelType = (Rec "TreeChannel" (InternalChoice $ Map.fromList [("Leaf", Skip),("Node", (Semi (Semi (Out IntType)(Var "TreeChannel")) (Var "TreeChannel")))]))

  describe "Simple tests" $ do
    it "Int" $ do
      (read "Int" :: BasicType) `shouldBe` IntType
    it "Char" $ do
      (read "Char" :: BasicType) `shouldBe` CharType
    it "Bool" $ do
      (read "Bool" :: BasicType) `shouldBe` BoolType
    it "Unit" $ do
      (read "()" :: BasicType) `shouldBe` UnitType
    it "Skip" $ do
      (read "Skip" :: Type) `shouldBe` Skip
    it "!Int;?Bool" $ do
      (read "!Int;?Bool" :: Type) `shouldBe` (Semi (Out IntType) (In BoolType))
    it "!Int" $ do
      (read "!Int" :: Type) `shouldBe` (Out IntType)
    it "?Int" $ do
      (read "?Int" :: Type) `shouldBe` (In IntType)
    it "Int->Int" $ do
      (read "Int->Int" :: Type) `shouldBe` (UnFun (Basic IntType) (Basic IntType))
    it "Int-oInt" $ do
      (read "Int-oInt" :: Type) `shouldBe` (LinFun (Basic IntType) (Basic IntType))
    it "(Int,Int)" $ do
      (read "(Int,Int)" :: Type) `shouldBe` (Pair (Basic IntType) (Basic IntType))
    it "&{a:?Int,b:!Bool}" $ do
      (read "&{a:?Int,b:!Bool}" :: Type) `shouldBe` (ExternalChoice (Map.fromList [("a",In IntType),("b",Out BoolType)]))
    it "+{a:!Int,b:?Bool}" $ do
      (read "+{a:!Int,b:?Bool}" :: Type) `shouldBe` (InternalChoice (Map.fromList [("a",Out IntType),("b",In BoolType)]))
    it "[a:Int,b:Bool]" $ do
      (read "[a:Int,b:Bool]" :: Type) `shouldBe` (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
    it "rec a.Bool" $ do
      (read "rec a.Bool" :: Type) `shouldBe` (Rec "a" (Basic BoolType))
    it "forall a.Bool" $ do
      (read "forall a.Bool" :: Type) `shouldBe` (Forall "a" (Basic BoolType))

  describe "Operator precedence" $ do
    it "(Char)" $ do
      (read "(Char)" :: Type) `shouldBe` (Basic CharType)
    it "(Skip)" $ do
      (read "(Skip)" :: Type) `shouldBe` Skip
    it "(?Bool)" $ do
      (read "(?Bool)" :: Type) `shouldBe` (In BoolType)
    it "(!Char)" $ do
      (read "(!Char)" :: Type) `shouldBe` (Out CharType)
    it "((Int,Char))" $ do
      (read "((Int,Char))" :: Type) `shouldBe` (Pair (Basic IntType) (Basic CharType))

  describe "Whitespaces" $ do
    it " Skip" $ do
      (read " Skip " :: Type) `shouldBe` Skip
    it " Int" $ do
      (read " Int " :: BasicType) `shouldBe`  IntType
    it "?Int ; !Bool" $ do
      (read " ?Int ; !Bool " :: Type) `shouldBe` (Semi (In IntType) (Out BoolType))
    it "Int -> Int" $ do
      (read " Int -> Int " :: Type) `shouldBe` (UnFun (Basic IntType) (Basic IntType))
    it "Int -o Int" $ do
      (read " Int -o Int " :: Type) `shouldBe` (LinFun (Basic IntType) (Basic IntType))
    it "( Int , Int )" $ do
      (read "( Int , Int )" :: Type) `shouldBe` (Pair (Basic IntType) (Basic IntType))
    it "rec a . a" $ do
      (read "rec a . a" :: Type) `shouldBe` (Rec "a" (Var "a"))
    it "+{i : !Int, b : !Bool}" $ do
      (read "+{i : !Int, b : !Bool}" :: Type) `shouldBe` (InternalChoice (Map.fromList [("i",Out IntType),("b",Out BoolType)]))

  describe "Nested operators" $ do
    it "((Int,Bool),Char)" $ do
      (read "((Int,Bool),Char)" :: Type) `shouldBe` (Pair (Pair (Basic IntType)(Basic BoolType)) (Basic CharType))
    it "rec a . (rec i . Int)" $ do
      (read "rec a . (rec i . Int)" :: Type) `shouldBe` (Rec "a" (Rec "i" (Basic IntType)))
    it "forall a.(forall b.Bool)" $ do
      (read "forall a.(forall b.Bool)" :: Type) `shouldBe` (Forall "a" (Forall "b" (Basic BoolType)))

  describe "Associativity" $ do
    it "forall f . f -o f -> f" $ do
      (read "forall f . f -o f -> f" :: Type) `shouldBe` (Forall "f" (LinFun (Var "f")(UnFun (Var "f")(Var "f"))))
    it "forall f . (f -o f) -> f" $ do
      (read "forall f . (f -o f) -> f" :: Type) `shouldBe` (Forall "f" (UnFun (LinFun (Var "f")(Var "f")) (Var "f")))
    it "Skip;Skip;Skip" $ do
      (read "Skip;Skip;Skip" :: Type) `shouldBe` (Semi (Semi Skip Skip) Skip)
    it "forall Internal . (Internal,Int)" $ do
      (read "forall Internal . (Internal,Int)" :: Type) `shouldBe` (Forall "Internal" (Pair (Var "Internal") (Basic IntType)))
    it "forall Skiper . (Skiper,Int)" $ do
      (read "forall Skiper . (Skiper,Int)" :: Type) `shouldBe` (Forall "Skiper" (Pair (Var "Skiper") (Basic IntType)))
    it "forall a . forall b . a -> {-A {-comment-} inside-} b" $ do
      (read "forall a . forall b . a -> {-A comment inside-} b" :: Type) `shouldBe` (Forall "a" (Forall "b" (UnFun (Var "a") (Var "b"))))


--  evaluate (read "" :: Type) `shouldThrow` anyException
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
      (read ("forall beta ."++treeChannelRead++"->("++treeChannelRead++";beta)->beta") :: Type) `shouldBe`
        (Forall "beta"  (UnFun treeChannelType (UnFun (Semi treeChannelType (Var "beta")) (Var "beta"))))

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
      (read ("forall alpha . " ++ treeChannelRead ++ "->(" ++ xFormChanRead ++ ";alpha)->("++treeChannelRead++",alpha)")) `shouldBe`
        (Forall "alpha" (UnFun treeChannelType (UnFun (Semi xFormChanType (Var "alpha")) (Pair treeChannelType (Var "alpha")))))
              -- (Semi (UnFun treeChannelType xFormChanType) (UnFun (Var "alpha") (Pair treeChannelType (Var "alpha")))))
        -- (Forall "alpha" (Semi (UnFun treeChannelType xFormChanType) (UnFun (Var "alpha") (Pair treeChannelType (Var "alpha")))))

    it "treeSum" $ do
      (read ("forall alpha . (" ++ xFormChanDualRead ++ ";alpha) -o (Int, alpha) ")) `shouldBe`
        (Forall "alpha" (LinFun (Semi xFormChanDualType  (Var "alpha")) (Pair (Basic IntType) (Var "alpha"))))

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
      (read ("(" ++ termChanDualRead ++ ";!Int)->Skip") :: Type) `shouldBe`  (UnFun (Semi termChanDualType (Out IntType)) Skip)

    it "receiveEval" $ do
      (read ("forall alpha .(" ++ termChanDualRead ++ ";!Int)->(Int, alpha)") :: Type) `shouldBe`
            (Forall "alpha" (UnFun (Semi termChanDualType (Out IntType))(Pair (Basic IntType)(Var "alpha"))))

    it "client" $ do
      (read ("(" ++ termChanRead ++ ";?Int)->(Int,Skip)") :: Type) `shouldBe` (UnFun (Semi termChanType (In IntType))(Pair (Basic IntType) Skip))

  describe "Lazy tree traversal (Listing 4)" $ do
    let xploreTreeChanRead = "(rec XformChan . +{Leaf:Skip,Node:!Int;XformChan;XformChan;?Int})"
    let xploreTreeChanType = (Rec "XformChan" (InternalChoice $ Map.fromList ([("Leaf", Skip), ("Node", (Semi(Semi(Semi (Out IntType)(Var "XformChan")) (Var "XformChan")) (In IntType)))])))

    let xploreNodeChanRead = "(rec XploreNodeChan . +{Value:!Int;XploreNodeChan, Left:"++xploreTreeChanRead++";XploreNodeChan,Right:"++xploreTreeChanRead++";XploreNodeChan,Exit:Skip})"

    let xploreNodeChanType = (Rec "XploreNodeChan" (InternalChoice $ Map.fromList ([("Value", (Semi (Out IntType)(Var "XploreNodeChan"))),
                                                                                    ("Left", (Semi (xploreTreeChanType)(Var "XploreNodeChan"))),
                                                                                    ("Right", (Semi (xploreTreeChanType)(Var "XploreNodeChan"))),
                                                                                    ("Exit", Skip)
                                                                                   ])))
    it "xploreTreeChan" $ do
      (read (xploreTreeChanRead) :: Type) `shouldBe` (xploreTreeChanType)

    it "xploreNodeChan" $ do
      (read (xploreNodeChanRead) :: Type) `shouldBe` (xploreNodeChanType)

    it "exploreTree" $ do
      (read ("rec alpha . Int -> "++ treeChannelRead ++"->"++ treeChannelRead ++"->(" ++ xploreNodeChanRead ++ ";alpha)->alpha") :: Type)  `shouldBe`
          (Rec "alpha" (UnFun (Basic IntType) (UnFun treeChannelType (UnFun treeChannelType (UnFun (Semi xploreNodeChanType (Var "alpha")) (Var "alpha"))))))


        -- (Rec "alpha" (Semi (UnFun (Basic IntType) (UnFun (treeChannelType) (UnFun (treeChannelType)(xploreNodeChanType))))
        --                   (UnFun (Var "alpha")(Var "alpha"))))
