-- runTestTT allTests
-- ghc -fhpc TestParse.hs --make
-- ./TestParse
-- hpc report TestParse --exclude=Main --exclude=QC
-- hpc markup TestParse --exclude=Main --exclude=QC
module Types.TestParse(allTests) where

import Types.Parser
import Test.HUnit
--TYPEMAP
import qualified Data.Map.Strict as Map
import SpecHelper

test1 = TestCase (assertEqual "for (read \"Int\")," IntType (read "Int" :: BasicType))
test2 = TestCase (assertEqual "for (read \"Char\")," CharType (read "Char" :: BasicType))
test3 = TestCase (assertEqual "for (read \"Bool\")," BoolType (read "Bool" :: BasicType))
test4 = TestCase (assertEqual "for (read \"Unit\")," UnitType (read "()" :: BasicType))

-- TESTING TYPES
-- Skip | Semi Type Type | Out BasicType | In BasicType | Basic BasicType |
-- UnFun Type Type | LinFun Type Type | Pair Type Type | ExternalChoice TypeMap |
-- InternalChoice TypeMap | Datatype TypeMap | Rec String Type | Forall String Type | Var String |

test5 = TestCase (assertEqual "for (read \"Skip\")," Skip (read "Skip" :: Type))
test6 = TestCase (assertEqual "for (read \"Int;Bool\")," (Semi (Basic IntType) (Basic BoolType)) (read "Int;Bool" :: Type))
test7 = TestCase (assertEqual "for (read \"!Int\")," (Out IntType) (read "!Int" :: Type))
test8 = TestCase (assertEqual "for (read \"?Int\")," (In IntType) (read "?Int" :: Type))
test9 = TestCase (assertEqual "for (read \"Int->Int\")," (UnFun (Basic IntType) (Basic IntType)) (read "Int->Int" :: Type))
test10 = TestCase (assertEqual "for (read \"Int-oInt\")," (LinFun (Basic IntType) (Basic IntType)) (read "Int-oInt" :: Type))
test11 = TestCase (assertEqual "for (read \"(Int,Int)\")," (Pair (Basic IntType) (Basic IntType)) (read "(Int,Int)" :: Type))

test12 = TestCase (assertEqual "for (read \"&{a:Int,b:Bool}\"),"
      (ExternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)])) (read "&{a:Int,b:Bool}" :: Type))
test13 = TestCase (assertEqual "for (read \"+{a:Int,b:Bool}\"),"
      (InternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)])) (read "+{a:Int,b:Bool}" :: Type))
test14 = TestCase (assertEqual "for (read \"[a:Int,b:Bool]\"),"
      (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)])) (read "[a:Int,b:Bool]" :: Type))

test15 = TestCase (assertEqual "for (read \"rec a.Bool\")," (Rec "a" (Basic BoolType)) (read "rec a.Bool" :: Type))
test16 = TestCase (assertEqual "for (read \"forall a.Bool\")," (Forall "a" (Basic BoolType)) (read "forall a.Bool" :: Type))
test17 = TestCase (assertEqual "for (read \"z\")," (Var "z") (read "z" :: Type))

-- Precedence
test18 = TestCase (assertEqual "for (read \"(Char)\")," (Basic CharType) (read "(Char)" :: Type))
test19 = TestCase (assertEqual "for (read \"(Skip)\")," Skip (read "(Skip)" :: Type))
test20 = TestCase (assertEqual "for (read \"(?Bool)\")," (In BoolType) (read "(?Bool)" :: Type))
test21 = TestCase (assertEqual "for (read \"(!Char)\")," (Out CharType) (read "(!Char)" :: Type))
test22 = TestCase (assertEqual "for (read \"((Int,Char))\")," (Pair (Basic IntType) (Basic CharType)) (read "((Int,Char))" :: Type))

-- Test if the whitespaces are escaped
test23 = TestCase (assertEqual "for (read \" Skip\")," Skip (read " Skip " :: Type))
test24 = TestCase (assertEqual "for (read \" Int\")," IntType (read " Int " :: BasicType))
test25 = TestCase (assertEqual "for (read \"Int ; Bool\")," (Semi (Basic IntType) (Basic BoolType)) (read " Int ; Bool " :: Type))
test26 = TestCase (assertEqual "for (read \"Int -> Int\")," (UnFun (Basic IntType) (Basic IntType)) (read " Int -> Int " :: Type))
test27 = TestCase (assertEqual "for (read \"Int -o Int\")," (LinFun (Basic IntType) (Basic IntType)) (read " Int -o Int " :: Type))
test28 = TestCase (assertEqual "for (read \"( Int , Int )\")," (Pair (Basic IntType) (Basic IntType)) (read "( Int , Int )" :: Type))
test29 = TestCase (assertEqual "for (read \"rec a . A\")," (Rec "a" (Var "A")) (read "rec a . A" :: Type))

test30 = TestCase (assertEqual "for (read \"+{i : Int, b : Bool}\"),"
      (InternalChoice (Map.fromList [("i",Basic IntType),("b",Basic BoolType)])) (read "+{i : Int, b : Bool}" :: Type))

--more complex structures
test31 = TestCase (assertEqual "for (read \"((Int,Bool),a)\")," (Pair (Pair (Basic IntType)(Basic BoolType)) (Var "a")) (read "((Int,Bool),a)" :: Type))
test32 = TestCase (assertEqual "for (read \"rec a . (rec i . Int)\")," (Rec "a" (Rec "i" (Basic IntType))) (read "rec a . (rec i . Int)" :: Type))
test33 = TestCase (assertEqual "for (read \"forall a.(forall b.Bool)\")," (Forall "a" (Forall "b" (Basic BoolType))) (read "forall a.(forall b.Bool)" :: Type))

-- associativity
test34 = TestCase (assertEqual "for (read \"f -o f -> f\")," (LinFun (Var "f")(UnFun (Var "f")(Var "f"))) (read "f -o f -> f" :: Type))
test35 = TestCase (assertEqual "for (read \"(f -o f) -> f\")," (UnFun (LinFun (Var "f")(Var "f")) (Var "f")) (read "(f -o f) -> f" :: Type))
test36 = TestCase (assertEqual "for (read \"Skip;Skip;Skip\")," (Semi (Semi Skip Skip) Skip) (read "Skip;Skip;Skip" :: Type))

test37 = TestCase (assertEqual "for (read \"(Internal,Int)\")," (Pair (Var "Internal") (Basic IntType)) (read "(Internal,Int)" :: Type))
test38 = TestCase (assertEqual "for (read \"(Skiper,Int)\")," (Pair (Var "Skiper") (Basic IntType)) (read "(Skiper,Int)" :: Type))
test39 = TestCase (assertEqual "for (read \"a -> {-A comment inside-} b\")," (UnFun (Var "a") (Var "b")) (read "a -> {-A comment inside-} b" :: Type))


validTests = "Testing Parser" ~: TestList [TestLabel "for (read \"Int\")" test1,
                      TestLabel "for (read \"Char\")" test2,
                      TestLabel "for (read \"Bool\")" test3,
                      TestLabel "for (read \"Unit\")" test4,
                      TestLabel "for (read \"Skip\")" test5,
                      TestLabel "for (read \"Int;Bool\")" test6,
                      TestLabel "for (read \"!Int\")" test7,
                      TestLabel "for (read \"?Int\")" test8,
                      TestLabel "for (read \"Int->Int\")" test9,
                      TestLabel "for (read \"Int-oInt\")" test10,
                      TestLabel "for (read \"(Int,Int)\")" test11,
                      TestLabel "for (read \"&{a:Int,b:Bool}\")" test12,
                      TestLabel "for (read \"+{a:Int,b:Bool}\")" test13,
                      TestLabel "for (read \"[a:Int,b:Bool]\")" test14,
                      TestLabel "for (read \"rec a.Bool\")" test15,
                      TestLabel "for (read \"forall a.Bool\")" test16,
                      TestLabel "for (read \"z\")" test17,
                      TestLabel "for (read \"(Char)\")" test18,
                      TestLabel "for (read \"(Skip)\")" test19,
                      TestLabel "for (read \"(?Bool)\")" test20,
                      TestLabel "for (read \"(!Char)\")" test21,
                      TestLabel "for (read \"((Int,Char))\")" test22,
                      TestLabel "for (read \" Skip\")" test23,
                      TestLabel "for (read \" Int\")" test24,
                      TestLabel "for (read \"Int ; Bool\")" test25,
                      TestLabel "for (read \"Int -> Int\")" test26,
                      TestLabel "for (read \"Int -o Int\")" test27,
                      TestLabel "for (read \"( Int , Int )\")" test28,
                      TestLabel "for (read \"rec a . A\")" test29,
                      TestLabel "for (read \"+{i : Int, b : Bool}\")" test30,
                      TestLabel "for (read \"((Int,Bool),a)\")" test31,
                      TestLabel "for (read \"rec a . (rec i . Int)\")" test32,
                      TestLabel "for (read \"forall a.(forall b.Bool)\")" test33,
                      TestLabel "for (read \"f -o f -> f\")" test34,
                      TestLabel "for (read \"(f -o f) -> f\")" test35,
                      TestLabel "for (read \"Skip;Skip;Skip\")" test36,
                      TestLabel "for (read \"(Internal,Int)\")" test37,
                      TestLabel "for (read \"(Skiper,Int)\")" test38,
                      TestLabel "for (read \"a -> {-A comment inside-} b\")" test39
                       -- TestLabel "test40" test40
                      ]

--invalidTests = TestList [TestLabel "test5" test4]
--allTests = TestList[validTests , invalidTests]
allTests = TestList[validTests]

-- shortcut to run the tests
runAll = runTestTT allTests
runValid = runTestTT validTests
--runInvalid = runTestTT invalidTests

-- runTestTT allTests
-- main :: IO ()
-- main = do
--     runAll
--     return ()

-- TODO: test eq and show
