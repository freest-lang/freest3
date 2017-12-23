module Types.TestKinding (allTests) where

import SpecHelper
import Types.Kinding
import Test.HUnit
import qualified Data.Map.Strict as Map

-- Test contractivity
-- Simple tests
test1 = TestCase (assertBool "contractive Map.empty (UnFun (Basic IntType)(Basic BoolType))" True)
test2 = TestCase (assertBool "contractive Map.empty (LinFun (Basic IntType)(Basic BoolType))" True)
test3 = TestCase (assertBool "contractive Map.empty (Pair (Basic IntType)(Basic BoolType))" True)

test4 = TestCase (assertEqual "contractive Map.empty (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
                      (contractive Map.empty (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test5 = TestCase (assertBool "contractive Map.empty (Basic IntType)" True )
test6 = TestCase (assertBool "contractive Map.empty (Out IntType)" True)

test7 = TestCase (assertEqual "for (In BoolType)" True (contractive Map.empty (In BoolType)))

test8 = TestCase (assertEqual "for (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
          (contractive Map.empty (ExternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test9 = TestCase (assertEqual "for (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
          (contractive Map.empty (InternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test10 = TestCase (assertBool "contractive Map.empty (Skip)" True)
test11 = TestCase (assertBool "contractive Map.empty (Semi (Out IntType)(In BoolType))" True)

test12 = TestCase (assertEqual "for (Semi (Var \"x\")(In BoolType))" False
          (contractive Map.empty (Semi (Var "x")(In BoolType))))

test13 = TestCase (assertEqual "for (Rec \"x\" (In BoolType))" True
          (contractive Map.empty (Rec "x" (In BoolType))))

test14 = TestCase (assertEqual "for (Var \"x\")" False (contractive Map.empty (Var "x")))

test15 = TestCase (assertEqual "for (Var \"x\")" True (contractive (Map.fromList [("x",(Kind Session Lin))]) (Var "x")))

test16 = TestCase (assertEqual "for (Forall \"a\" (Basic BoolType))" True
          (contractive Map.empty (Forall "a" (Basic BoolType))))

test17 = TestCase (assertEqual "for (Forall \"a\" (Var \"x\"))" False
            (contractive Map.empty (Forall "a" (Var "x"))))

-- Paper examples:
test18 = TestCase (assertEqual "for (Rec \"x\" (Semi Skip (Var \"x\")))," True
            (contractive Map.empty (Rec "x" (Semi Skip (Var "x")))))

test19 = TestCase (assertEqual "for (Rec \"x\" (Semi (Var \"x\") (Out IntType)))" False
            (contractive Map.empty (Rec "x" (Semi (Var "x") (Out IntType)))))

test20 = TestCase (assertEqual "for (Rec \"x\" (Semi (Out IntType) (Var \"x\")))" True
            (contractive Map.empty (Rec "x" (Semi (Out IntType) (Var "x")))))

test21 = TestCase (assertEqual "for (Rec \"x\" (Rec \"y\" (Semi (Var \"x\") (Var \"y\"))))" False
            (contractive Map.empty (Rec "x" (Rec "y" (Semi (Var "x") (Var "y"))))))

test22 = TestCase (assertEqual "for (Rec \"x\" (Semi (Out Int) (Rec \"y\" (Semi (Var \"x\") (Var \"y\")))))" True
            (contractive Map.empty (Rec "x" (Semi (Out IntType)(Rec "y" (Semi (Var "x") (Var "y")))))))

-- Test Kinding system

test23 = TestCase (assertEqual "kindOf (UnFun (Basic IntType)(Basic BoolType))" (Kind Arbitrary Un) (kindOf (UnFun (Basic IntType)(Basic BoolType))))
test24 = TestCase (assertEqual "kindOf (LinFun (Basic IntType)(Basic BoolType))" (Kind Arbitrary Lin) (kindOf ((LinFun (Basic IntType)(Basic BoolType)))))
test25 = TestCase (assertEqual "kindOf (Pair (Basic IntType)(Basic BoolType))" (Kind Arbitrary Lin) (kindOf (Pair (Basic IntType)(Basic BoolType))))
test26 = TestCase (assertEqual "kindOf (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" (Kind Arbitrary Un) (kindOf (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))
test27 = TestCase (assertEqual "kindOf (Pair (Basic IntType)(Basic BoolType))" (Kind Arbitrary Lin) (kindOf (Pair (Basic IntType)(Basic BoolType))))
test28 = TestCase (assertEqual "kindOf (Basic IntType)" (Kind Arbitrary Un) (kindOf (Basic IntType)))
test29 = TestCase (assertEqual "kindOf (Out IntType)" (Kind Session Lin) (kindOf (Out IntType)))

test30 = TestCase (assertEqual "kindOf (In BoolType)" (Kind Session Lin) (kindOf (In BoolType)))


test31 = TestCase (assertEqual "kindOf (Skip)" (Kind Session Un) (kindOf Skip))
test32 = TestCase (assertEqual "kindOf (Semi (Out IntType)(In BoolType))" (Kind Session Lin) (kindOf (Semi (Out IntType)(In BoolType))))




{--



test13 = TestCase (assertEqual "for (Rec \"x\" (In BoolType))" True
          (contractive Map.empty (Rec "x" (In BoolType))))

test14 = TestCase (assertEqual "for (Var \"x\")" False (contractive Map.empty (Var "x")))

test15 = TestCase (assertEqual "for (Var \"x\")" True (contractive (Map.fromList [("x",(Kind Session Lin))]) (Var "x")))

--- negative
test31 = TestCase (assertEqual "kindOf (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" (Kind Session Lin)
          (kindOf (ExternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test32 = TestCase (assertEqual "kindOf (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" (Kind Session Lin)
          (kindOf (InternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test33 = TestCase (assertEqual "kindOf (Semi (Var \"x\")(In BoolType))" (Kind Session Lin) (kindOf (Semi (Var "x")(In BoolType))))

--}


validTests = "Kinding & Contractivity Unit tests" ~:TestList [
                      TestLabel "for (UnFun (Basic IntType)(Basic BoolType))"  test1,
                      TestLabel "for (LinFun (Basic IntType)(Basic BoolType))"  test2,
                      TestLabel "for (Pair (Basic IntType)(Basic BoolType))"  test3,
                      TestLabel "for (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test4,
                      TestLabel "for (Basic IntType)"  test5,
                      TestLabel "for (Out IntType)"  test6,
                      TestLabel "for (In BoolType)"  test7,
                      TestLabel "for (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test8,
                      TestLabel "for (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test9,
                      TestLabel "for (Skip)"  test10,
                      TestLabel "for (Semi (Out IntType)(In BoolType))"  test11,
                      TestLabel "for (Semi (Var \"x\")(In BoolType))"  test12,
                      TestLabel "for (Rec \"x\" (In BoolType))"  test13,
                      TestLabel "for (Var \"x\")"  test14,
                      TestLabel "for (Var \"x\")"  test15,
                      TestLabel "for (Forall \"a\" (Basic BoolType))"  test16,
                      TestLabel "for (Forall \"a\" (Var \"x\"))"  test17,
                      TestLabel "for (Rec \"x\" (Semi Skip (Var \"x\")))"  test18,
                      TestLabel "for (Rec \"x\" (Semi (Var \"x\") (Out IntType)))"  test19,
                      TestLabel "for (Rec \"x\" (Semi (Out IntType) (Var \"x\")))"  test20,
                      TestLabel "for (Rec \"x\" (Rec \"y\" (Semi (Var \"x\") (Var \"y\"))))"  test21,
                      TestLabel "for (Rec \"x\" (Semi (Out Int) (Rec \"y\" (Semi (Var \"x\") (Var \"y\")))))"  test22,
                      TestLabel "kindOf (UnFun (Basic IntType)(Basic BoolType))"  test23,
                      TestLabel "kindOf (LinFun (Basic IntType)(Basic BoolType))"  test24,
                      TestLabel "kindOf (Pair (Basic IntType)(Basic BoolType))"  test25,
                      TestLabel "kindOf (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test26,
                      TestLabel "kindOf (Pair (Basic IntType)(Basic BoolType))"  test27,
                      TestLabel "kindOf (Basic IntType)"  test28,
                      TestLabel "kindOf (Out IntType)"  test29,
                      TestLabel "kindOf (In BoolType)"  test30,
                      TestLabel "kindOf (Skip)" test31,
                      TestLabel "kindOf (Semi (Out IntType)(In BoolType))" test32

                      -- TestLabel "kindOf (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test31,
                      -- TestLabel "kindOf (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test32

                       ]

--invalidTests = TestList [TestLabel "test5" test4]
--allTests = TestList[validTests , invalidTests]
allTests = TestList[validTests]

-- shortcut to run the tests
runAll = runTestTT allTests
runValid = runTestTT validTests
--runInvalid = runTestTT invalidTests
