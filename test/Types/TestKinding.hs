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

test7 = TestCase (assertEqual "contractive Map.empty (In BoolType)" True (contractive Map.empty (In BoolType)))

test8 = TestCase (assertEqual "contractive Map.empty (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
          (contractive Map.empty (ExternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test9 = TestCase (assertEqual "contractive Map.empty (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
          (contractive Map.empty (InternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test10 = TestCase (assertBool "contractive Map.empty (Skip)" True)
test11 = TestCase (assertBool "contractive Map.empty (Semi (Out IntType)(In BoolType))" True)

test12 = TestCase (assertEqual "contractive Map.empty (Semi (Var \"x\")(In BoolType))" False
          (contractive Map.empty (Semi (Var "x")(In BoolType))))

test13 = TestCase (assertEqual "contractive Map.empty (Rec \"x\" (In BoolType))" True
          (contractive Map.empty (Rec "x" (In BoolType))))

test14 = TestCase (assertEqual "contractive Map.empty (Var \"x\")" False (contractive Map.empty (Var "x")))

test15 = TestCase (assertEqual "contractive Map.empty (Var \"x\")" True (contractive (Map.fromList [("x",(Kind Session Lin))]) (Var "x")))

test16 = TestCase (assertEqual "contractive Map.empty (Forall \"a\" (Basic BoolType))" True
          (contractive Map.empty (Forall "a" (Basic BoolType))))

test17 = TestCase (assertEqual "contractive Map.empty (Forall \"a\" (Var \"x\"))" False
            (contractive Map.empty (Forall "a" (Var "x"))))

-- Paper examples:
test18 = TestCase (assertEqual "contractive Map.empty (Rec \"x\" (Semi Skip (Var \"x\")))," True
            (contractive Map.empty (Rec "x" (Semi Skip (Var "x")))))

test19 = TestCase (assertEqual "contractive Map.empty (Rec \"x\" (Semi (Var \"x\") (Out IntType)))" False
            (contractive Map.empty (Rec "x" (Semi (Var "x") (Out IntType)))))

test20 = TestCase (assertEqual "contractive Map.empty (Rec \"x\" (Semi (Out IntType) (Var \"x\")))" True
            (contractive Map.empty (Rec "x" (Semi (Out IntType) (Var "x")))))

test21 = TestCase (assertEqual "contractive Map.empty (Rec \"x\" (Rec \"y\" (Semi (Var \"x\") (Var \"y\"))))" False
            (contractive Map.empty (Rec "x" (Rec "y" (Semi (Var "x") (Var "y"))))))

test22 = TestCase (assertEqual "contractive Map.empty (Rec \"x\" (Semi (Out Int) (Rec \"y\" (Semi (Var \"x\") (Var \"y\")))))" True
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
test33 = TestCase (assertEqual "kindOf (Rec \"x\" (In BoolType))" (Kind Session Lin) (kindOf (Rec "x" (In BoolType))))
test34 = TestCase (assertEqual "kindOf (Forall \"a\" (Basic BoolType))" (Kind Arbitrary Un) (kindOf (Forall "a" (Basic BoolType))))

--Paper kinding examples
test35 = TestCase (assertEqual "kindOf (Rec \"x\" (Semi Skip Skip)))" (Kind Session Un) (kindOf (Rec "x" (Semi Skip Skip))))
test36 = TestCase (assertEqual "kindOf (Rec \"x\" (Semi (Semi (Out IntType) Skip) (In IntType))))" (Kind Session Lin) (kindOf (Rec "x" (Semi (Semi (Out IntType) Skip) (In IntType)))))



-- TODO: ERROR
-- Paper examples:
-- testXX = TestCase (assertEqual "kindOf (Rec \"x\" (Semi Skip (Var \"x\")))," (Kind Session Lin) (kindOf (Rec "x" (Semi Skip (Var "x")))))
--
-- test19 = TestCase (assertEqual "kindOf (Rec \"x\" (Semi (Var \"x\") (Out IntType)))" (Kind...) (kindOf (Rec "x" (Semi (Var "x") (Out IntType)))))
--
-- test20 = TestCase (assertEqual "kindOf (Rec \"x\" (Semi (Out IntType) (Var \"x\")))" (Kind...) (kindOf (Rec "x" (Semi (Out IntType) (Var "x")))))
--
-- test21 = TestCase (assertEqual "kindOf (Rec \"x\" (Rec \"y\" (Semi (Var \"x\") (Var \"y\"))))" (Kind...) (kindOf (Rec "x" (Rec "y" (Semi (Var "x") (Var "y"))))))
--
-- test22 = TestCase (assertEqual "kindOf (Rec \"x\" (Semi (Out Int) (Rec \"y\" (Semi (Var \"x\") (Var \"y\")))))" (Kind...) (kindOf (Rec "x" (Semi (Out IntType)(Rec "y" (Semi (Var "x") (Var "y")))))))


{--
test13 = TestCase (assertEqual "contractive Map.empty (Rec \"x\" (In BoolType))" True
          (contractive Map.empty (Rec "x" (In BoolType))))

test14 = TestCase (assertEqual "contractive Map.empty (Var \"x\")" False (contractive Map.empty (Var "x")))

test15 = TestCase (assertEqual "contractive Map.empty (Var \"x\")" True (contractive (Map.fromList [("x",(Kind Session Lin))]) (Var "x")))

--- negative
test31 = TestCase (assertEqual "kindOf (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" (Kind Session Lin)
          (kindOf (ExternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test32 = TestCase (assertEqual "kindOf (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" (Kind Session Lin)
          (kindOf (InternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test33 = TestCase (assertEqual "kindOf (Semi (Var \"x\")(In BoolType))" (Kind Session Lin) (kindOf (Semi (Var "x")(In BoolType))))

test35 = TestCase (assertEqual "kindOf (Forall \"a\" (Var \"x\"))" (Kind Arbitrary Un) (kindOf (Forall "a" (Var "x"))))

--}


validTests = "Kinding & Contractivity Unit tests" ~:TestList [
                      TestLabel "contractive Map.empty (UnFun (Basic IntType)(Basic BoolType))"  test1,
                      TestLabel "contractive Map.empty (LinFun (Basic IntType)(Basic BoolType))"  test2,
                      TestLabel "contractive Map.empty (Pair (Basic IntType)(Basic BoolType))"  test3,
                      TestLabel "contractive Map.empty (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test4,
                      TestLabel "contractive Map.empty (Basic IntType)"  test5,
                      TestLabel "contractive Map.empty (Out IntType)"  test6,
                      TestLabel "contractive Map.empty (In BoolType)"  test7,
                      TestLabel "contractive Map.empty (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test8,
                      TestLabel "contractive Map.empty (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test9,
                      TestLabel "contractive Map.empty (Skip)"  test10,
                      TestLabel "contractive Map.empty (Semi (Out IntType)(In BoolType))"  test11,
                      TestLabel "contractive Map.empty (Semi (Var \"x\")(In BoolType))"  test12,
                      TestLabel "contractive Map.empty (Rec \"x\" (In BoolType))"  test13,
                      TestLabel "contractive Map.empty (Var \"x\")"  test14,
                      TestLabel "contractive Map.empty (Var \"x\")"  test15,
                      TestLabel "contractive Map.empty (Forall \"a\" (Basic BoolType))"  test16,
                      TestLabel "contractive Map.empty (Forall \"a\" (Var \"x\"))"  test17,
                      TestLabel "contractive Map.empty (Rec \"x\" (Semi Skip (Var \"x\")))"  test18,
                      TestLabel "contractive Map.empty (Rec \"x\" (Semi (Var \"x\") (Out IntType)))"  test19,
                      TestLabel "contractive Map.empty (Rec \"x\" (Semi (Out IntType) (Var \"x\")))"  test20,
                      TestLabel "contractive Map.empty (Rec \"x\" (Rec \"y\" (Semi (Var \"x\") (Var \"y\"))))"  test21,
                      TestLabel "contractive Map.empty (Rec \"x\" (Semi (Out Int) (Rec \"y\" (Semi (Var \"x\") (Var \"y\")))))"  test22,
                      TestLabel "kindOf (UnFun (Basic IntType)(Basic BoolType))"  test23,
                      TestLabel "kindOf (LinFun (Basic IntType)(Basic BoolType))"  test24,
                      TestLabel "kindOf (Pair (Basic IntType)(Basic BoolType))"  test25,
                      TestLabel "kindOf (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test26,
                      TestLabel "kindOf (Pair (Basic IntType)(Basic BoolType))"  test27,
                      TestLabel "kindOf (Basic IntType)"  test28,
                      TestLabel "kindOf (Out IntType)"  test29,
                      TestLabel "kindOf (In BoolType)"  test30,
                      TestLabel "kindOf (Skip)" test31,
                      TestLabel "kindOf (Semi (Out IntType)(In BoolType))" test32,
                      TestLabel "kindOf (Rec \"x\" (In BoolType))" test33,
                      TestLabel "kindOf (Forall \"a\" (Basic BoolType))" test34,
                      TestLabel "kindOf (Rec \"x\" (Semi Skip Skip)))" test35,
                      TestLabel "kindOf (Rec \"x\" (Semi (Semi (Out IntType) Skip) (In IntType))))" test36


                      -- TestLabel "kindOf ()" test3
                      -- TestLabel "kindOf (ExternalChoice (Map.fromList [(\"a  D \",Basic IntType),(\"b\",Basic BoolType)]))"  test31,
                      -- TestLabel "kindOf (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))"  test32

                       ]

--invalidTests = TestList [TestLabel "test5" test4]
--allTests = TestList[validTests , invalidTests]
allTests = TestList[validTests]

-- shortcut to run the tests
runAll = runTestTT allTests
runValid = runTestTT validTests
--runInvalid = runTestTT invalidTests
