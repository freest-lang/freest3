-- runTestTT allTests
-- main
-- ghc -fhpc TestParse.hs --make
-- ./TestParse
-- hpc report TestParse --exclude=Main --exclude=QC
-- hpc markup TestParse --exclude=Main --exclude=QC

import Types
import Parser
import Test.HUnit
--TYPEMAP
import qualified Data.Map.Strict as Map

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

-- TODO : Datatype
--test14 = Datatype TypeMap
test15 = TestCase (assertEqual "for (read \"rec a.Bool\")," (Rec "a" (Basic BoolType)) (read "rec a.Bool" :: Type))
test16 = TestCase (assertEqual "for (read \"forall a.Bool\")," (Forall "a" (Basic BoolType)) (read "forall a.Bool" :: Type))
test17 = TestCase (assertEqual "for (read \"z\")," (Var "z") (read "z" :: Type))

-- TODO: Parser error )fix
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
test28 = TestCase (assertEqual "for (read \"( Int , Int )\")," (Pair (Basic IntType) (Basic IntType)) (read "( Int , Int )" :: Type)) -- TODO: Parser error )fix
test29 = TestCase (assertEqual "for (read \"rec a . A\")," (Rec "a" (Var "A")) (read "rec a . A" :: Type))

test30 = TestCase (assertEqual "for (read \"+{i : Int, b : Bool}\"),"
      (InternalChoice (Map.fromList [("i",Basic IntType),("b",Basic BoolType)])) (read "+{i : Int, b : Bool}" :: Type))

-- read "+{i : Int, b : Bool}" :: Type

-- test40 = TestCase (assertEqual "for (read \" Skip ; Skip \")," (Semi Skip Skip) (read " Skip ; Skip " :: Type))




--TODO:
-- test try
-- test pairs of pairs
-- rec rec
-- forall forall
-- -> ->
-- -o -o
-- ; ;
-- maybe InternalChoice and ExternalChoice
-- comments: nested comments; comments inside types; block and line comments
-- test not followed parser
-- ex:
--  read "(Internal,Int)" :: Type - Pair (Var "Internal") (Basic IntType)
-- read "(Skiper,Int)" :: Type
-- TESTAR parser notFollowedBy nos operadores -o e ->
-- parens type combinated with other to evaluate precedence


validTests = TestList [TestLabel "test1" test1,
                       TestLabel "test2" test2,
                       TestLabel "test3" test3,
                       TestLabel "test4" test4,
                       TestLabel "test5" test5,
                       TestLabel "test6" test6,
                       TestLabel "test7" test7,
                       TestLabel "test8" test8,
                       TestLabel "test9" test9,
                       TestLabel "test10" test10,
                       TestLabel "test11" test11,
                       TestLabel "test12" test12,
                       TestLabel "test13" test13,
                       -- TestLabel "test14" test14,
                       TestLabel "test15" test15,
                       TestLabel "test16" test16,
                       TestLabel "test17" test17,
                       TestLabel "test18" test18,
                       TestLabel "test19" test19,
                       TestLabel "test20" test20,
                       TestLabel "test21" test21,
                       TestLabel "test22" test22,
                       TestLabel "test23" test23,
                       TestLabel "test24" test24,
                       TestLabel "test25" test25,
                       TestLabel "test26" test26,
                       TestLabel "test27" test27,
                       TestLabel "test28" test28,
                       TestLabel "test29" test29,
                       TestLabel "test30" test30
                      ]

--invalidTests = TestList [TestLabel "test5" test4]
--allTests = TestList[validTests , invalidTests]
allTests = TestList[validTests]

-- shortcut to run the tests
runAll = runTestTT allTests
runValid = runTestTT validTests
--runInvalid = runTestTT invalidTests

-- runTestTT allTests
main :: IO ()
main = do
    runAll
    return ()

-- INVALID TESTS

-- Invalid whitespaces
-- testX = TestCase (assertEqual "for (read \"! Int\")," (Out IntType) (read "! Int" :: Type))
-- testX = TestCase (assertEqual "for (read \"? Int\")," (In IntType) (read "? Int" :: Type))
