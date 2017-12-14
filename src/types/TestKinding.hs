import Types
import Kinding
import Test.HUnit

import qualified Data.Map.Strict as Map

-- Test contractivity

-- Simple tests
test1 = TestCase (assertEqual "for (UnFun (Basic IntType)(Basic BoolType))" True
          (contractive Map.empty (UnFun (Basic IntType)(Basic BoolType))))

test2 = TestCase (assertEqual "for (LinFun (Basic IntType)(Basic BoolType))" True
          (contractive Map.empty (LinFun (Basic IntType)(Basic BoolType))))

test3 = TestCase (assertEqual "for (Pair (Basic IntType)(Basic BoolType))" True
          (contractive Map.empty (Pair (Basic IntType)(Basic BoolType))))

test4 = TestCase (assertEqual "for (Datatype (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
          (contractive Map.empty (Datatype (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test5 = TestCase (assertEqual "for (Basic IntType)" True (contractive Map.empty (Basic IntType)))

test6 = TestCase (assertEqual "for (Out IntType)" True (contractive Map.empty (Out IntType)))

test7 = TestCase (assertEqual "for (In BoolType)" True (contractive Map.empty (In BoolType)))

test8 = TestCase (assertEqual "for (ExternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
          (contractive Map.empty (ExternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test9 = TestCase (assertEqual "for (InternalChoice (Map.fromList [(\"a\",Basic IntType),(\"b\",Basic BoolType)]))" True
          (contractive Map.empty (InternalChoice (Map.fromList [("a",Basic IntType),("b",Basic BoolType)]))))

test10 = TestCase (assertEqual "for (Skip)" True (contractive Map.empty (Skip)))

test11 = TestCase (assertEqual "for (Semi (Out IntType)(In BoolType))" True
          (contractive Map.empty (Semi (Out IntType)(In BoolType))))

test12 = TestCase (assertEqual "for (Semi (Var \"x\")(In BoolType))" False
          (contractive Map.empty (Semi (Var "x")(In BoolType))))

test13 = TestCase (assertEqual "for (Rec \"x\" (In BoolType))" True
          (contractive Map.empty (Rec "x" (In BoolType))))

test14 = TestCase (assertEqual "for (Var \"x\")" False (contractive Map.empty (Var "x")))

test15 = TestCase (assertEqual "for (Var \"x\")" True (contractive (Map.fromList [("x",(Kind Session Lin))]) (Var "x")))

-- test1 = TestCase (assertEqual "" True (contractive Map.empty ()))
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
--contractive Map.empty (Rec "x" (Semi Skip (Var "x")))

-- should be false
-- contractive Map.empty (Forall "a" (Rec "x" (Semi (Var "x") (Out IntType))))


--Rec "x" (Rec "y" (Semi (Var "x") (Var "y")))

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
                       TestLabel "test14" test14,
                       TestLabel "test15" test15,
                       TestLabel "test16" test16,
                       TestLabel "test17" test17,
                       TestLabel "test18" test18,
                       TestLabel "test19" test19,
                       TestLabel "test20" test20,
                       TestLabel "test21" test21,
                       TestLabel "test22" test22
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
