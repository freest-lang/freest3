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
  -- TODO: later should work ?
  -- let treeChannelRead = "(rec TreeChannel . +{Leaf:Skip,Node:!Int;TreeChannel;TreeChannel})"
  -- let treeChannelType = (Rec (Bind "TreeChannel" (Kind Session Lin)) (Choice Internal $ Map.fromList [("Leaf", Skip), ("Node", (Semi (Semi (Message Out IntType) (Var "TreeChannel")) (Var "TreeChannel")))]))
  let treeChannelRead = "rec treeChannel . +{Leaf:Skip,Node:!Int;treeChannel;treeChannel}"
  let treeChannelType = (Rec (Bind "treeChannel" (Kind Session Lin)) (Choice Internal $ Map.fromList [("Leaf", Skip), ("Node", (Semi (Semi (Message Out IntType) (Var "treeChannel")) (Var "treeChannel")))]))


  describe "Simple tests" $ do
    it "Int" $ do
      (read "Int" :: Type) `shouldBe` (Basic IntType)
    it "Char" $ do
      (read "Char" :: Type) `shouldBe` (Basic CharType)
    it "Bool" $ do
      (read "Bool" :: Type) `shouldBe` (Basic BoolType)
    it "Unit" $ do
      (read "()" :: Type) `shouldBe` (Basic UnitType)
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

   -- TODO: Later should work
    -- it "&{a:?Int,b:!Bool}" $ do
    --   (read "&{a:?Int,b:!Bool}" :: Type) `shouldBe` (Choice External (Map.fromList [("a",Message In IntType),("b",Message Out BoolType)]))
    -- it "+{a:!Int,b:?Bool}" $ do
    --   (read "+{a:!Int,b:?Bool}" :: Type) `shouldBe` (Choice Internal (Map.fromList [("a",Message Out IntType),("b",Message In BoolType)]))
    -- it "[a:Int,b:Bool]" $ do
    --   (read "[a:Int,b:Bool]" :: Type) `shouldBe` (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))
    it "&{A:?Int,A:!Bool}" $ do
      (read "&{A:?Int,B:!Bool}" :: Type) `shouldBe` (Choice External (Map.fromList [("A",Message In IntType),("B",Message Out BoolType)]))
    it "+{A:!Int,B:?Bool}" $ do
      (read "+{A:!Int,B:?Bool}" :: Type) `shouldBe` (Choice Internal (Map.fromList [("A",Message Out IntType),("B",Message In BoolType)]))
    it "[A:Int,B:Bool]" $ do
      (read "[A:Int,B:Bool]" :: Type) `shouldBe` (Datatype (Map.fromList [("A",Basic IntType),("B",Basic BoolType)]))
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
      (read " Int " :: Type) `shouldBe`  (Basic IntType)
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
        -- TODO: later should work
    -- it "+{i : !Int, b : !Bool}" $ do
    --   (read "+{i : !Int, b : !Bool}" :: Type) `shouldBe` (Choice Internal (Map.fromList [("i",Message Out IntType),("b",Message Out BoolType)]))

    it "+{I : !Int, B : !Bool}" $ do
      (read "+{I : !Int, B : !Bool}" :: Type) `shouldBe` (Choice Internal (Map.fromList [("I",Message Out IntType),("B",Message Out BoolType)]))
    
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
        (read ("rec alpha . ("++treeChannelRead++";alpha)") :: Type) `shouldBe`
          (Rec (Bind "alpha" (Kind Session Lin)) (Semi (treeChannelType) (Var "alpha")))

    it "rec alpha . !int;TreeChannel;TreeChannel;alpha" $ do
      (read ("rec alpha . (!Int;" ++ treeChannelRead ++ ";" ++ treeChannelRead ++ ";alpha)") :: Type) `shouldBe`
        (Rec (Bind "alpha" (Kind Session Lin))
          (Semi (Semi (Semi (Message Out IntType) treeChannelType) treeChannelType) (Var "alpha"))
        )
         -- (Semi (Message Out IntType) (Semi treeChannelType (Semi treeChannelType (Var "alpha")))))

    it "rec alpha . ((!int;(TreeChannel;TreeChannel));alpha)" $ do
      (read ("rec alpha . (!Int;("++treeChannelRead++";"++treeChannelRead++");alpha)") :: Type)
        `shouldBe`
         (Rec (Bind "alpha" (Kind Session Lin))
                (Semi (Semi (Message Out IntType) (Semi treeChannelType treeChannelType))
                       (Var "alpha")))

    it "rec alpha . (!int;((TreeChannel;TreeChannel);alpha))" $ do
      (read ("rec alpha . (!Int;(("++treeChannelRead++";"++treeChannelRead++");alpha))") :: Type) `shouldBe`
        (Rec (Bind "alpha" (Kind Session Lin)) (Semi (Message Out IntType) (Semi (Semi treeChannelType treeChannelType) (Var "alpha"))))

    it "rec alpha . ((TreeChannel;TreeChannel);alpha))" $ do
      (read ("rec alpha . (("++treeChannelRead++";"++treeChannelRead++");alpha)") :: Type) `shouldBe`
        (Rec (Bind "alpha" (Kind Session Lin)) (Semi (Semi treeChannelType treeChannelType) (Var "alpha")))

    it "rec alpha . (TreeChannel;(TreeChannel;alpha))" $ do
      (read ("rec alpha . ("++treeChannelRead++";("++treeChannelRead++";alpha))") :: Type) `shouldBe`
        (Rec (Bind "alpha" (Kind Session Lin)) (Semi treeChannelType (Semi  treeChannelType (Var "alpha"))))

  describe "Remote tree transformation (Listing 2)" $ do
    let xFormChanRead = "rec xFormChan . +{Leaf:Skip,Node:!Int;xFormChan;xFormChan;?Int}"
    let xFormChanType = (Rec (Bind "xFormChan" (Kind Session Lin))
                         (Choice Internal $ Map.fromList
                          ([("Leaf",Skip),
                            ("Node", (Semi (Semi (Semi (Message Out IntType)(Var "xFormChan"))
                                            (Var "xFormChan")) (Message In IntType)))])))

    let xFormChanDualRead = "rec xFormChan . &{Leaf:Skip,Node:?Int;xFormChan;xFormChan;!Int}"
    let xFormChanDualType = (Rec (Bind "xFormChan" (Kind Session Lin))
                             (Choice External $ Map.fromList
                              ([("Leaf",Skip),
                                ("Node", (Semi (Semi (Semi (Message In IntType)(Var "xFormChan"))
                                                (Var "xFormChan")) (Message Out IntType)))])))
    it "xFormChan Type" $ do
      (read (xFormChanRead) :: Type) `shouldBe` xFormChanType

    it "xFormChan Dual Type" $ do
      (read (xFormChanDualRead) :: Type) `shouldBe` xFormChanDualType

  describe "Arithmetic expression server (Listing 3)" $ do
    let termChanRead = "rec termChan . +{Const:!Int,Add:termChan;termChan,Mult:termChan;termChan}"
    let termChanType = (Rec (Bind "termChan" (Kind Session Lin)) (Choice Internal $ Map.fromList ([("Const",(Message Out IntType)),("Add",(Semi (Var "termChan")(Var "termChan"))),
                                                                                           ("Mult",(Semi (Var "termChan")(Var "termChan")))])))

    let termChanDualRead = "rec termChan . &{Const:?Int,Add:termChan;termChan,Mult:termChan;termChan}"
    let termChanDualType = (Rec (Bind "termChan" (Kind Session Lin)) (Choice External $ Map.fromList ([("Const",(Message In IntType)),("Add",(Semi (Var "termChan")(Var "termChan"))),("Mult",(Semi (Var "termChan")(Var "termChan")))])))

    it "TermChan Type" $ do
      (read (termChanRead) :: Type) `shouldBe` termChanType

    it "TermChan Dual Type" $ do
      (read (termChanDualRead) :: Type) `shouldBe` termChanDualType

    it "computeService" $ do
      (read ("(" ++ termChanDualRead ++ ";!Int)->Skip") :: Type) `shouldBe`  (Fun Un (Semi termChanDualType (Message Out IntType)) Skip)

  
    it "client" $ do
      (read ("(" ++ termChanRead ++ ";?Int)->(Int,Skip)") :: Type) `shouldBe` (Fun Un (Semi termChanType (Message In IntType))(PairType (Basic IntType) Skip))

  describe "Lazy tree traversal (Listing 4)" $ do
    let xploreTreeChanRead = "rec xFormChan . +{Leaf:Skip,Node:!Int;xFormChan;xFormChan;?Int}"
    let xploreTreeChanType = (Rec (Bind "xFormChan" (Kind Session Lin))
                              (Choice Internal $ Map.fromList
                               ([("Leaf", Skip),
                                 ("Node", (Semi (Semi (Semi (Message Out IntType) (Var "xFormChan"))
                                                 (Var "xFormChan")) (Message In IntType)))])))

    let xploreNodeChanRead = "rec xPloreNodeChan . +{Value:!Int;xPloreNodeChan, Left:"++xploreTreeChanRead++";xPloreNodeChan,Right:"++xploreTreeChanRead++";xPloreNodeChan,Exit:Skip}"

    let xploreNodeChanType = (Rec (Bind "xPloreNodeChan" (Kind Session Lin)) (Choice Internal $ Map.fromList ([("Value", (Semi (Message Out IntType)(Var "xPloreNodeChan"))),("Left", (Semi (xploreTreeChanType)(Var "xPloreNodeChan"))), ("Right", (Semi (xploreTreeChanType)(Var "xPloreNodeChan"))),("Exit", Skip)])))
    
    it "xploreTreeChan" $ do
      (read (xploreTreeChanRead) :: Type) `shouldBe` (xploreTreeChanType)

    it "xploreNodeChan" $ do
      (read (xploreNodeChanRead) :: Type) `shouldBe` (xploreNodeChanType)

    it "exploreTree" $ do
      (read ("rec alpha . (Int -> "++ treeChannelRead ++"->"++ treeChannelRead ++"->(" ++ xploreNodeChanRead ++ ";alpha)->alpha)") :: Type)  `shouldBe`
          (Rec (Bind "alpha" (Kind Session Lin)) (Fun Un (Basic IntType) (Fun Un treeChannelType (Fun Un treeChannelType (Fun Un (Semi xploreNodeChanType (Var "alpha")) (Var "alpha"))))))


        -- (Rec "alpha" (Semi (Fun Un (Basic IntType) (Fun Un (treeChannelType) (Fun Un (treeChannelType)(xploreNodeChanType))))
        --                   (Fun Un (Var "alpha")(Var "alpha"))))
