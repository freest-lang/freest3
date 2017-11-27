import Types
import Parser
import Test.HUnit

test1 = TestCase (assertEqual "for (read \"Skip\")," Skip (read "Skip" :: Type))

test2 = TestCase (assertEqual "for (read \"Int\")," IntType (read "Int" :: BasicType))

test3 = TestCase (assertEqual "for (read \"z\")," (Var "z") (read "z" :: Type))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]

-- runTestTT tests
