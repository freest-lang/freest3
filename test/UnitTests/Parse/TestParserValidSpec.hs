module Parse.TestParserValidSpec(spec) where

import           Parse.Lexer (defaultPos)
import           SpecHelper
import           Control.Exception (evaluate)
import           Syntax.Kinds
import           Syntax.Bind
import qualified Data.Map.Strict as Map

-- Just to be able to run it alone
main :: IO ()
main = hspec spec

kBind :: String -> KBind
kBind x = KBind defaultPos x (Kind defaultPos Session Lin)

spec :: Spec
spec = do
  
  let p = defaultPos
  let treeChannelRead = "rec treeChannel . +{Leaf:Skip,Node:!Int;treeChannel;treeChannel}"
  let treeChannelType =
        (Rec p (kBind "treeChannel")
          (Choice p Out $ Map.fromList
           [(Bind p "Leaf", Skip p),
            (Bind p "Node", (Semi p (Semi p (Message p Out IntType) (Var p "treeChannel")) (Var p "treeChannel")))]))

  describe "Simple tests" $ do
    it "Int" $ do
      (read "Int" :: Type) `shouldBe` (Basic p IntType)
    it "Char" $ do
      (read "Char" :: Type) `shouldBe` (Basic p CharType)
    it "Bool" $ do
      (read "Bool" :: Type) `shouldBe` (Basic p BoolType)
    it "Unit" $ do
      (read "()" :: Type) `shouldBe` (Basic p UnitType)
    it "Skip" $ do
      (read "Skip" :: Type) `shouldBe` (Skip p)
    it "!Int;?Bool" $ do
      (read "!Int;?Bool" :: Type) `shouldBe` (Semi p (Message p Out IntType) (Message p In BoolType))
    it "!Int" $ do
      (read "!Int" :: Type) `shouldBe` (Message p Out IntType)
    it "?Int" $ do
      (read "?Int" :: Type) `shouldBe` (Message p In IntType)
    it "Int->Int" $ do
      (read "Int->Int" :: Type) `shouldBe` (Fun p Un (Basic p IntType) (Basic p IntType))
    it "Int-oInt" $ do
      (read "Int-oInt" :: Type) `shouldBe` (Fun p Lin (Basic p IntType) (Basic p IntType))
    it "(Int,Int)" $ do
      (read "(Int,Int)" :: Type) `shouldBe` (PairType p (Basic p IntType) (Basic p IntType))

    it "&{A:?Int,A:!Bool}" $ do
      (read "&{A:?Int,B:!Bool}" :: Type) `shouldBe` (Choice p In (Map.fromList [(Bind p "A",Message p In IntType),(Bind p "B",Message p Out BoolType)]))      
    it "+{A:!Int,B:?Bool}" $ do
      (read "+{A:!Int,B:?Bool}" :: Type) `shouldBe` (Choice p Out (Map.fromList [(Bind p "A",Message p Out IntType),(Bind p "B",Message p In BoolType)]))
    it "[A:Int,B:Bool]" $ do
      (read "[A:Int,B:Bool]" :: Type) `shouldBe` (Datatype p (Map.fromList [(Bind p "A",Basic p IntType),(Bind p "B",Basic p BoolType)]))
    it "rec a.Bool" $ do
      (read "rec a.Bool" :: Type) `shouldBe` (Rec p (kBind "a") (Basic p BoolType))
 
  describe "Operator precedence" $ do
    it "(Char)" $ do
      (read "(Char)" :: Type) `shouldBe` (Basic p CharType)
    it "(Skip)" $ do
      (read "(Skip)" :: Type) `shouldBe` (Skip p)
    it "(?Bool)" $ do
      (read "(?Bool)" :: Type) `shouldBe` (Message p In BoolType)
    it "(!Char)" $ do
      (read "(!Char)" :: Type) `shouldBe` (Message p Out CharType)
    it "((Int,Char))" $ do
      (read "((Int,Char))" :: Type) `shouldBe` (PairType p (Basic p IntType) (Basic p CharType))

  describe "Whitespaces" $ do
    it " Skip" $ do
      (read " Skip " :: Type) `shouldBe` (Skip p)
    it " Int" $ do
      (read " Int " :: Type) `shouldBe`  (Basic p IntType)
    it "?Int ; !Bool" $ do
      (read " ?Int ; !Bool " :: Type) `shouldBe` (Semi p (Message p In IntType) (Message p Out BoolType))
    it "Int -> Int" $ do
      (read " Int -> Int " :: Type) `shouldBe` (Fun p Un (Basic p IntType) (Basic p IntType))
    it "Int -o Int" $ do
      (read " Int -o Int " :: Type) `shouldBe` (Fun p Lin (Basic p IntType) (Basic p IntType))
    it "( Int , Int )" $ do
      (read "( Int , Int )" :: Type) `shouldBe` (PairType p (Basic p IntType) (Basic p IntType))
    it "rec a . a" $ do
      (read "rec a . a" :: Type) `shouldBe` (Rec p (kBind "a") (Var p "a"))

    it "+{I : !Int, B : !Bool}" $ do
      (read "+{I : !Int, B : !Bool}" :: Type) `shouldBe` (Choice p Out (Map.fromList [(Bind p "I",Message p Out IntType),(Bind p "B",Message p Out BoolType)]))
    
  describe "Nested operators" $ do
    it "((Int,Bool),Char)" $ do
      (read "((Int,Bool),Char)" :: Type) `shouldBe` (PairType p (PairType p (Basic p IntType)(Basic p BoolType)) (Basic p CharType))
    it "rec a . (rec i . Int)" $ do
      (read "rec a . (rec i . Int)" :: Type) `shouldBe` (Rec p (kBind "a") (Rec p (kBind "i") (Basic p IntType)))

--  evaluate (read "" :: Type) `shouldThrow` anyException
  describe "Paper examples (more complex tests)" $ do

    it "TreeChannel" $ do
      (read  treeChannelRead :: Type) `shouldBe` treeChannelType

    it "TreeChannel;alpha" $ do
        (read ("rec alpha . ("++treeChannelRead++";alpha)") :: Type) `shouldBe`
          (Rec p (kBind "alpha") (Semi p (treeChannelType) (Var p "alpha")))

    it "rec alpha . !int;TreeChannel;TreeChannel;alpha" $ do
      (read ("rec alpha . (!Int;" ++ treeChannelRead ++ ";" ++ treeChannelRead ++ ";alpha)") :: Type) `shouldBe`
        (Rec p (kBind "alpha") (Semi p (Semi p (Semi p (Message p Out IntType) treeChannelType) treeChannelType) (Var p "alpha")))

    it "rec alpha . ((!int;(TreeChannel;TreeChannel));alpha)" $ do
      (read ("rec alpha . (!Int;("++treeChannelRead++";"++treeChannelRead++");alpha)") :: Type)
        `shouldBe`
         (Rec p (kBind "alpha")
                (Semi p (Semi p (Message p Out IntType) (Semi p treeChannelType treeChannelType))
                       (Var p "alpha")))

    it "rec alpha . (!int;((TreeChannel;TreeChannel);alpha))" $ do
      (read ("rec alpha . (!Int;(("++treeChannelRead++";"++treeChannelRead++");alpha))") :: Type) `shouldBe`
        (Rec p (kBind "alpha") (Semi p (Message p Out IntType) (Semi p (Semi p treeChannelType treeChannelType) (Var p "alpha"))))

    it "rec alpha . ((TreeChannel;TreeChannel);alpha))" $ do
      (read ("rec alpha . (("++treeChannelRead++";"++treeChannelRead++");alpha)") :: Type) `shouldBe`
        (Rec p (kBind "alpha") (Semi p (Semi p treeChannelType treeChannelType) (Var p "alpha")))

    it "rec alpha . (TreeChannel;(TreeChannel;alpha))" $ do
      (read ("rec alpha . ("++treeChannelRead++";("++treeChannelRead++";alpha))") :: Type) `shouldBe`
        (Rec p (kBind "alpha") (Semi p treeChannelType (Semi p  treeChannelType (Var p "alpha"))))

  describe "Remote tree transformation (Listing 2)" $ do
    let xFormChanRead = "rec xFormChan . +{Leaf:Skip,Node:!Int;xFormChan;xFormChan;?Int}"
    let xFormChanType = (Rec p (kBind "xFormChan")
                         (Choice p Out $ Map.fromList
                          ([(Bind p "Leaf",Skip p),
                            (Bind p "Node", (Semi p (Semi p (Semi p (Message p Out IntType)(Var p "xFormChan"))
                                            (Var p "xFormChan")) (Message p In IntType)))])))

    let xFormChanDualRead = "rec xFormChan . &{Leaf:Skip,Node:?Int;xFormChan;xFormChan;!Int}"
    let xFormChanDualType = (Rec p (kBind "xFormChan")
                             (Choice p In $ Map.fromList
                              ([(Bind p "Leaf",Skip p),
                                (Bind p "Node", (Semi p (Semi p (Semi p (Message p In IntType)(Var p "xFormChan"))
                                                (Var p "xFormChan")) (Message p Out IntType)))])))
    it "xFormChan Type" $ do
      (read (xFormChanRead) :: Type) `shouldBe` xFormChanType
      
    it "xFormChan Dual Type" $ do
      (read (xFormChanDualRead) :: Type) `shouldBe` xFormChanDualType


  describe "Arithmetic expression server (Listing 3)" $ do
    let termChanRead = "rec termChan . +{Const:!Int,Add:termChan;termChan,Mult:termChan;termChan}"
    let termChanType = (Rec p (kBind "termChan")
                        (Choice p Out $ Map.fromList (
                            [(Bind p "Const",(Message p Out IntType)),
                             (Bind p "Add",(Semi p (Var p "termChan")(Var p "termChan"))),
                             (Bind p "Mult",(Semi p (Var p "termChan")(Var p "termChan")))])))

    let termChanDualRead = "rec termChan . &{Const:?Int,Add:termChan;termChan,Mult:termChan;termChan}"
    let termChanDualType = (Rec p (kBind "termChan")
                            (Choice p In $ Map.fromList (
                                [(Bind p "Const",(Message p In IntType)),
                                 (Bind p "Add",(Semi p (Var p "termChan")(Var p "termChan"))),
                                 (Bind p "Mult",(Semi p (Var p "termChan")(Var p "termChan")))])))

    it "TermChan Type" $ do
      (read (termChanRead) :: Type) `shouldBe` termChanType

    it "TermChan Dual Type" $ do
      (read (termChanDualRead) :: Type) `shouldBe` termChanDualType

    it "computeService" $ do
      (read ("(" ++ termChanDualRead ++ ";!Int)->Skip") :: Type) `shouldBe`  (Fun p Un (Semi p termChanDualType (Message p Out IntType)) (Skip p))

  
    it "client" $ do
      (read ("(" ++ termChanRead ++ ";?Int)->(Int,Skip)") :: Type) `shouldBe` (Fun p Un (Semi p termChanType (Message p In IntType))(PairType p (Basic p IntType) (Skip p)))



  describe "Lazy tree traversal (Listing 4)" $ do
    let xploreTreeChanRead = "rec xFormChan . +{Leaf:Skip,Node:!Int;xFormChan;xFormChan;?Int}"
    let xploreTreeChanType = (Rec p (kBind "xFormChan")
                              (Choice p Out $ Map.fromList
                               ([(Bind p "Leaf", Skip p),
                                 (Bind p "Node", (Semi p (Semi p (Semi p (Message p Out IntType) (Var p "xFormChan")) (Var p "xFormChan")) (Message p In IntType)))])))

    let xploreNodeChanRead = "rec xPloreNodeChan . +{Value:!Int;xPloreNodeChan, Left:"++xploreTreeChanRead++";xPloreNodeChan,Right:"++xploreTreeChanRead++";xPloreNodeChan,Exit:Skip}"

    let xploreNodeChanType =
          (Rec p (kBind "xPloreNodeChan")
           (Choice p Out $ Map.fromList (
               [(Bind p "Value", (Semi p (Message p Out IntType)(Var p "xPloreNodeChan"))),
                (Bind p "Left", (Semi p (xploreTreeChanType)(Var p "xPloreNodeChan"))),
                (Bind p "Right", (Semi p (xploreTreeChanType)(Var p "xPloreNodeChan"))),
                (Bind p "Exit", Skip p)])))
    
    it "xploreTreeChan" $ do
      (read (xploreTreeChanRead) :: Type) `shouldBe` (xploreTreeChanType)

    it "xploreNodeChan" $ do
      (read (xploreNodeChanRead) :: Type) `shouldBe` (xploreNodeChanType)

    it "exploreTree" $ do
      (read ("rec alpha . (Int -> "++ treeChannelRead ++"->"++ treeChannelRead ++"->(" ++ xploreNodeChanRead ++ ";alpha)->alpha)") :: Type)  `shouldBe`
          (Rec p (kBind "alpha") (Fun p Un (Basic p IntType) (Fun p Un treeChannelType (Fun p Un treeChannelType (Fun p Un (Semi p xploreNodeChanType (Var p "alpha")) (Var p "alpha"))))))
