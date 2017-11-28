import Types
import Parser
import Test.HUnit

test1 = TestCase (assertEqual "for (read \"Skip\")," Skip (read "Skip" :: Type))

test2 = TestCase (assertEqual "for (read \"Int\")," IntType (read "Int" :: BasicType))

test3 = TestCase (assertEqual "for (read \"z\")," (Var "z") (read "z" :: Type))

test4 = TestCase (assertEqual "for (read \" Skip ; Skip \")," (Semi Skip Skip) (read " Skip ; Skip " :: Type))

test5 = TestCase (assertEqual "for (read \"! Int\")," (Out IntType) (read "! Int" :: Type))



validTests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3,  TestLabel "test4" test4]
invalidTests = TestList [TestLabel "test5" test5]
allTests = TestList[validTests , invalidTests]


-- runTestTT allTests
