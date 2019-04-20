{-# LANGUAGE TypeSynonymInstances #-}

import           Test.QuickCheck -- (arbitrary, elements, sized, listOf, Arbitrary, Gen, maxSuccess, quickCheckWith, stdArgs, oneof)
import           Syntax.Types
import           Syntax.Kinds
import           Control.Monad
import           Syntax.Expressions
import           Syntax.Schemes
import           Validation.Kinding (synthetise)
-- import           Utils.PreludeLoader (prelude)
import           Control.Monad.State
import           Utils.FreestState
import           Syntax.Base
import           Equivalence.Equivalence
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import qualified Data.Map.Strict as Map

main = quickCheckWith stdArgs { maxSuccess = 100 } prop_dual

-- prop_show :: Type -> Bool
-- prop_show t = show (read (show t) :: Type) == show t

prop_dual :: Type -> Bool
prop_dual t = dual (dual t) == t

instance Arbitrary Multiplicity where
  arbitrary = elements [Un, Lin]

instance Arbitrary Polarity where
  arbitrary = elements [In, Out]

instance Arbitrary Pos where
  arbitrary = elements [defaultPos]

ids :: [String]
ids = ["x", "y", "z"]

instance Arbitrary TypeVar where
  arbitrary = arbitraryVar ids

choices :: [String]
choices = ["A", "B", "C"]

instance Arbitrary ProgVar where
  arbitrary = arbitraryVar choices

arbitraryVar :: Variable b => [String] -> Gen b
arbitraryVar ids = do
  id <- elements ids
  return $ mkVar defaultPos id

instance Arbitrary Kind where
  arbitrary = elements [kindTL defaultPos]

instance Arbitrary TypeVarBind where
  arbitrary = liftM3 TypeVarBind arbitrary arbitrary arbitrary

instance Arbitrary BasicType where
  arbitrary = elements [IntType, CharType, BoolType, UnitType]

instance Arbitrary Type where
  arbitrary = sized arbitrarySession

arbitrarySession :: Int -> Gen Type
arbitrarySession 0 = return (Skip defaultPos)
arbitrarySession n = oneof
  [ liftM3 Semi arbitrary (arbitrarySession (n `div` 4)) (arbitrarySession (n `div` 4))
  , liftM3 Choice arbitrary arbitrary (arbitraryTypeMap (n `div` 4))
  , liftM3 Rec arbitrary arbitrary (arbitrarySession (n `div` 4))
  ]

arbitraryTypeMap :: Int -> Gen TypeMap
arbitraryTypeMap n = do
  m <- listOf1 $ arbitraryField (n `div` 4)
  return $ Map.fromList m

arbitraryField :: Int -> Gen (ProgVar,Type)
arbitraryField n = do
    k <- arbitrary
    t <- arbitrarySession (n `div` 4)
    return (k, t)

{-

arbitraryType :: Int -> Gen Type
arbitraryType 0 = return (Skip arbitrary)
arbitraryType n = do
  t <- oneof [liftM Basic arbitrary,
              -- Skip,
              liftM3 Semi arbitrary (arbitraryType (n `div` 4)) (arbitraryType (n `div` 4)),
              liftM3 Fun arbitrary arbitrary (arbitraryType (n `div` 4)) (arbitraryType (n `div` 4)),
              liftM3 Pair arbitrary (arbitraryType (n `div` 4)) (arbitraryType (n `div` 4)),
              liftM2 Choice arbitrary arbitrary (arbitraryTypeMap (n `div` 4)),
              liftM2 Datatype arbitrary (arbitraryDatatypeTypeMap (n `div` 4)),
              liftM2 Rec arbitrary arbitrary (arbitraryType (n `div` 4)),
              liftM2 Forall (arbitraryId) (arbitraryType (n `div` 4)),
              liftM2 ProgVar arbitrary arbitrary
             ]
  if (isType t) then
    return t
  else
    arbitraryType (n `div` 4)

arbitraryId :: Gen TypeVarBind
arbitraryId = do
  ts <- listOf $ elements (['A','B','C'])
  if isValidId ts then
    return ts
  else
    arbitraryId

isValidId :: [Char] -> Bool
isValidId [] = False
isValidId "rec" = False
isValidId  "Skip" = False
isValidId  "Forall" = False
isValidId _ = True

-- TypeMap of session types for
arbitraryTypeMap :: Int -> Gen TypeMap
arbitraryTypeMap n = do
    m <- listOf $ (arbitraryField (n `div` 4))
    if null m then
      arbitraryTypeMap (n `div` 4)
    else
      return $ Map.fromList m

arbitraryField :: Int -> Gen (TypeVarBind,Type)
arbitraryField n = do
    f <- arbitraryId
    t <- (arbitrarySession (n `div` 4))
    return (f,t)

arbitrarySession :: Int -> Gen Type
arbitrarySession 0 = return (Skip defaultPos)
arbitrarySession n = do
  t <- oneof [
              liftM3 Semi arbitrary (arbitrarySession (n `div` 4)) (arbitrarySession (n `div` 4))
              ,liftM3 Choice arbitrary arbitrary (arbitraryTypeMap (n `div` 4))
              ,liftM3 Choice arbitrary arbitrary (arbitraryTypeMap (n `div` 4))
              ,liftM3 Rec arbitrary arbitrary (arbitraryType (n `div` 4))
             ]

  if (isSessionType t) then
    return t
  else
    arbitrarySession (n `div` 4)


-- Datatype
arbitraryDatatypeTypeMap :: Int -> Gen TypeMap
arbitraryDatatypeTypeMap n = do
    m <- listOf $ (arbitraryDatatypeBinding (n `div` 4))
    if m == [] then
      arbitraryDatatypeTypeMap (n `div` 4)
    else
      return $ Map.fromList m

arbitraryDatatypeBinding :: Int -> Gen (TypeVarBind,Type)
arbitraryDatatypeBinding n = do
    f <- arbitraryId
    t <- (arbitraryTypeArbitrary (n `div` 4))
    return (f,t)

arbitraryTypeArbitrary :: Int -> Gen Type
arbitraryTypeArbitrary n = do
  t <- (arbitraryType (n `div` 4))

  if (not (isSchemeType t)) then
    return t
  else
    arbitraryTypeArbitrary (n `div` 4)

isType :: KindEnv -> Type -> Bool
isType kEnv t = do
  s1 <- parseProgram initialState prelude
  let s2 k = execState (synthetise kEnv t) s1
  s <- get
  return null (errors s)

isSchemeType :: Type -> Bool
isSchemeType t = isType kEnv t
  where kEnv = Map.fromList [(mkVar defaultPos "α", kindTL), (mkVar defaultPos "β", kindTL),
                             (mkVar defaultPos "x", kindTL), (mkVar defaultPos "y", kindTL),
                             (mkVar defaultPos "z", kindTL),
                             (mkVar defaultPos "γ", kindTU), (mkVar defaultPos "δ", kindTU),
                             (mkVar defaultPos "ρ", kindSL), (mkVar defaultPos "τ", kindSL),
                             (mkVar defaultPos "φ", kindSU), (mkVar defaultPos "ψ", kindSU)]

-}
