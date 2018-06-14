module Parse.TestParserValidSpec(spec) where

import           SpecHelper
import           Control.Exception (evaluate)
import           Syntax.Kinds
import qualified Data.Map.Strict as Map
import           Test.Hspec.Expectations (anyException, shouldThrow)

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let treeChannelRead = "(rec TreeChannel . +{Leaf:Skip,Node:!Int;TreeChannel;TreeChannel})"
  let treeChannelType = (Rec (Bind "TreeChannel" (Kind Session Lin)) (Choice Internal $ Map.fromList [("Leaf", Skip),("Node", (Semi (Message Out IntType) (Semi(Var "TreeChannel") (Var "TreeChannel"))))]))

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
      (read "!Int;?Bool" :: Type) `shouldBe` (Semi (Message Out IntType) (Message In BoolType))
    it "!Int" $ do
      (read "!Int" :: Type) `shouldBe` (Message Out IntType)
    it "?Int" $ do
      (read "?Int" :: Type) `shouldBe` (Message In IntType)
    it "Int->Int" $ do
      (read "Int->Int" :: Type) `shouldBe` (Fun Un (Basic IntType) (Basic IntType))
    it "Int-oInt" $ do
      (read "Int-oInt" :: Type) `shouldBe` (Fun Lin (Basic IntType) (Basic IntType))
    it "(Int,Int)" $ do
      (read "(Int,Int)" :: Type) `shouldBe` (PairType (Basic IntType) (Basic IntType))
    it "&{a:?Int,b:!Bool}" $ do
      (read "&{a:?Int,b:!Bool}" :: Type) `shouldBe` (Choice External (Map.fromList [("a",Message In IntType),("b",Message Out BoolType)]))
    it "+{a:!Int,b:?Bool}" $ do
      (read "+{a:!Int,b:?Bool}" :: Type) `shouldBe` (Choice Internal (Map.fromList [("a",Message Out IntType),("b",Message In BoolType)]))
    it "[a:Int,b:Bool]" $ do
      (read "[a:Int,b:Bool]" :: Type) `shouldBe` (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
    it "rec a.Bool" $ do
      (read "rec a.Bool" :: Type) `shouldBe` (Rec (Bind "a" (Kind Session Lin)) (Basic BoolType))
 
  describe "Operator precedence" $ do
    it "(Char)" $ do
      (read "(Char)" :: Type) `shouldBe` (Basic CharType)
    it "(Skip)" $ do
      (read "(Skip)" :: Type) `shouldBe` Skip
    it "(?Bool)" $ do
      (read "(?Bool)" :: Type) `shouldBe` (Message In BoolType)
    it "(!Char)" $ do
      (read "(!Char)" :: Type) `shouldBe` (Message Out CharType)
    it "((Int,Char))" $ do
      (read "((Int,Char))" :: Type) `shouldBe` (PairType (Basic IntType) (Basic CharType))

  describe "Whitespaces" $ do
    it " Skip" $ do
      (read " Skip " :: Type) `shouldBe` Skip
    it " Int" $ do
      (read " Int " :: BasicType) `shouldBe`  IntType
    it "?Int ; !Bool" $ do
      (read " ?Int ; !Bool " :: Type) `shouldBe` (Semi (Message In IntType) (Message Out BoolType))
    it "Int -> Int" $ do
      (read " Int -> Int " :: Type) `shouldBe` (Fun Un (Basic IntType) (Basic IntType))
    it "Int -o Int" $ do
      (read " Int -o Int " :: Type) `shouldBe` (Fun Lin (Basic IntType) (Basic IntType))
    it "( Int , Int )" $ do
      (read "( Int , Int )" :: Type) `shouldBe` (PairType (Basic IntType) (Basic IntType))
    it "rec a . a" $ do
      (read "rec a . a" :: Type) `shouldBe` (Rec (Bind "a" (Kind Session Lin)) (Var "a"))
    it "+{i : !Int, b : !Bool}" $ do
      (read "+{i : !Int, b : !Bool}" :: Type) `shouldBe` (Choice Internal (Map.fromList [("i",Message Out IntType),("b",Message Out BoolType)]))

  describe "Nested operators" $ do
    it "((Int,Bool),Char)" $ do
      (read "((Int,Bool),Char)" :: Type) `shouldBe` (PairType (PairType (Basic IntType)(Basic BoolType)) (Basic CharType))
    it "rec a . (rec i . Int)" $ do
      (read "rec a . (rec i . Int)" :: Type) `shouldBe` (Rec (Bind "a" (Kind Session Lin)) (Rec (Bind "i" (Kind Session Lin)) (Basic IntType)))


--  evaluate (read "" :: Type) `shouldThrow` anyException
  describe "Paper examples (more complex tests)" $ do

    it "TreeChannel" $ do
      (read  treeChannelRead :: Type) `shouldBe` treeChannelType

    it "TreeChannel;alpha" $ do
        (read ("rec alpha . "++treeChannelRead++";alpha") :: Type) `shouldBe`
          (Rec (Bind "alpha" (Kind Session Lin)) (Semi (treeChannelType) (Var "alpha")))

    it "rec alpha . !int;TreeChannel;TreeChannel;alpha" $ do
      (read ("rec alpha . !Int;" ++ treeChannelRead ++ ";" ++ treeChannelRead ++ ";alpha") :: Type) `shouldBe`
        (Rec (Bind "alpha" (Kind Session Lin)) (Semi (Message Out IntType) (Semi treeChannelType (Semi treeChannelType (Var "alpha")))))

    it "rec alpha . (!int;(TreeChannel;TreeChannel));alpha" $ do
      (read ("rec alpha . !Int;("++treeChannelRead++";"++treeChannelRead++");alpha") :: Type) `shouldBe`
        (Rec (Bind "alpha" (Kind Session Lin)) (Semi (Message Out IntType) (Semi (Semi treeChannelType treeChannelType ) (Var "alpha"))))

    it "rec alpha . !int;((TreeChannel;TreeChannel);alpha)" $ do
      (read ("rec alpha . !Int;(("++treeChannelRead++";"++treeChannelRead++");alpha)") :: Type) `shouldBe`
        (Rec (Bind "alpha" (Kind Session Lin)) (Semi (Message Out IntType) (Semi (Semi treeChannelType treeChannelType) (Var "alpha"))))

    it "rec alpha . (TreeChannel;TreeChannel);alpha" $ do
      (read ("rec alpha . ("++treeChannelRead++";"++treeChannelRead++");alpha") :: Type) `shouldBe`
        (Rec (Bind "alpha" (Kind Session Lin)) (Semi (Semi treeChannelType treeChannelType) (Var "alpha")))

    it "rec alpha . TreeChannel;(TreeChannel;alpha)" $ do
      (read ("rec alpha ."++treeChannelRead++";("++treeChannelRead++";alpha)") :: Type) `shouldBe`
        (Rec (Bind "alpha" (Kind Session Lin)) (Semi treeChannelType (Semi  treeChannelType (Var "alpha"))))

  describe "Remote tree transformation (Listing 2)" $ do
    let xFormChanRead = "(rec xFormChan . +{Leaf:Skip,Node:!Int;xFormChan;xFormChan;?Int})"
    let xFormChanType = (Rec (Bind "xFormChan" (Kind Session Lin)) (Choice Internal $
                                                            Map.fromList ([("Leaf",Skip),
                                                                           ("Node",(Semi (Message Out IntType) (Semi (Var "xFormChan") (Semi (Var "xFormChan")(Message In IntType)))))])))

    let xFormChanDualRead = "(rec xFormChan . &{Leaf:Skip,Node:?Int;xFormChan;xFormChan;!Int})"
    let xFormChanDualType = (Rec (Bind "xFormChan" (Kind Session Lin)) (Choice External $
                                                                Map.fromList ([("Leaf",Skip),
                                                                               ("Node",(Semi (Message In IntType) (Semi (Var "xFormChan")
                                                                                                           (Semi (Var "xFormChan")(Message Out IntType)))))])))

    it "xFormChan Type" $ do
      (read (xFormChanRead) :: Type) `shouldBe` xFormChanType

    it "xFormChan Dual Type" $ do
      (read (xFormChanDualRead) :: Type) `shouldBe` xFormChanDualType

  describe "Arithmetic expression server (Listing 3)" $ do
    let termChanRead = "(rec TermChan . +{Const:!Int,Add:TermChan;TermChan,Mult:TermChan;TermChan} )"
    let termChanType = (Rec (Bind "TermChan" (Kind Session Lin)) (Choice Internal $ Map.fromList ([("Const",(Message Out IntType)),("Add",(Semi (Var "TermChan")(Var "TermChan"))),
                                                                                           ("Mult",(Semi (Var "TermChan")(Var "TermChan")))])))

    let termChanDualRead = "(rec TermChan . &{Const:?Int,Add:TermChan;TermChan,Mult:TermChan;TermChan} )"
    let termChanDualType = (Rec (Bind "TermChan" (Kind Session Lin)) (Choice External $ Map.fromList ([("Const",(Message In IntType)),("Add",(Semi (Var "TermChan")(Var "TermChan"))),
                                                                                               ("Mult",(Semi (Var "TermChan")(Var "TermChan")))])))

    it "TermChan Type" $ do
      (read (termChanRead) :: Type) `shouldBe` termChanType

    it "TermChan Dual Type" $ do
      (read (termChanDualRead) :: Type) `shouldBe` termChanDualType

    it "computeService" $ do
      (read ("(" ++ termChanDualRead ++ ";!Int)->Skip") :: Type) `shouldBe`  (Fun Un (Semi termChanDualType (Message Out IntType)) Skip)

  
    it "client" $ do
      (read ("(" ++ termChanRead ++ ";?Int)->(Int,Skip)") :: Type) `shouldBe` (Fun Un (Semi termChanType (Message In IntType))(PairType (Basic IntType) Skip))

  describe "Lazy tree traversal (Listing 4)" $ do
    let xploreTreeChanRead = "(rec XformChan . +{Leaf:Skip,Node:!Int;XformChan;XformChan;?Int})"
    let xploreTreeChanType = (Rec (Bind "XformChan" (Kind Session Lin)) (Choice Internal $
                                                                 Map.fromList ([("Leaf", Skip),
                                                                                ("Node", (Semi (Message Out IntType) (Semi (Var "XformChan") (Semi (Var "XformChan") (Message In IntType)))))])))

    let xploreNodeChanRead = "(rec XploreNodeChan . +{Value:!Int;XploreNodeChan, Left:"++xploreTreeChanRead++";XploreNodeChan,Right:"++xploreTreeChanRead++";XploreNodeChan,Exit:Skip})"

    let xploreNodeChanType = (Rec (Bind "XploreNodeChan" (Kind Session Lin)) (Choice Internal $ Map.fromList ([("Value", (Semi (Message Out IntType)(Var "XploreNodeChan"))),
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
          (Rec (Bind "alpha" (Kind Session Lin)) (Fun Un (Basic IntType) (Fun Un treeChannelType (Fun Un treeChannelType (Fun Un (Semi xploreNodeChanType (Var "alpha")) (Var "alpha"))))))


        -- (Rec "alpha" (Semi (Fun Un (Basic IntType) (Fun Un (treeChannelType) (Fun Un (treeChannelType)(xploreNodeChanType))))
        --                   (Fun Un (Var "alpha")(Var "alpha"))))
