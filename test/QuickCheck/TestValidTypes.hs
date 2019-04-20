import           Test.QuickCheck (arbitrary, elements, sized, listOf, Arbitrary, Gen, maxSuccess, quickCheckWith, stdArgs, oneof)
import           Syntax.Types
import           Parse.Parser
import           Syntax.Kinds
import           Control.Monad
import           Syntax.Expressions
import           Syntax.Schemes
import           Validation.Kinding (synthetise)
import           Utils.PreludeLoader (prelude)
import           Control.Monad.State
import           Utils.FreestState
import           Syntax.Base
import qualified Data.Map.Strict as Map

main = quickCheckWith stdArgs { maxSuccess = 100 } prop_show

prop_show :: Type -> Bool
prop_show t = show (read (show t) :: Type) == show t

prop_dual :: Type -> Bool
prop_dual t = dual (dual t) == t

instance Arbitrary BasicType where
  arbitrary = elements [IntType, CharType, BoolType, UnitType]

instance Arbitrary Type where
    arbitrary = sized arbitraryType
    -- arbitrary = sized arbitrarySession

arbitraryType :: Int -> Gen Type
arbitraryType 0 = return (Skip defaultPos)
arbitraryType n = do
  t <- oneof [liftM Basic arbitrary,
              -- Skip,
              liftM2 Semi (arbitraryType (n `div` 4)) (arbitraryType (n `div` 4)),
              liftM Choice Out arbitrary,
              liftM Choice In arbitrary,
              liftM2 Fun Un (arbitraryType (n `div` 4)) (arbitraryType (n `div` 4)),
              liftM2 Fun Lin (arbitraryType (n `div` 4)) (arbitraryType (n `div` 4)),
              liftM2 Pair (arbitraryType (n `div` 4)) (arbitraryType (n `div` 4)),
              liftM Choice Out (arbitraryTypeMap (n `div` 4)),
              liftM Choice In (arbitraryTypeMap (n `div` 4)),
              liftM Datatype(arbitraryDatatypeTypeMap (n `div` 4)),
              liftM2 Rec (arbitraryId) (arbitraryType (n `div` 4)),
              -- liftM2 Forall (arbitraryId) (arbitraryType (n `div` 4)),
              liftM ProgVar arbitraryId
             ]
  if (isType t) then
    return t
  else
    arbitraryType (n `div` 4)

arbitraryId :: Gen TypeVarBind
arbitraryId = do
  ts <- listOf $ elements (['A'..'Z'] ++ ['a'..'z'])
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
    m <- listOf $ (arbitraryBinding (n `div` 4))
    if m == [] then
      arbitraryTypeMap (n `div` 4)
    else
      return $ Map.fromList m

arbitraryBinding :: Int -> Gen (TypeVarBind,Type)
arbitraryBinding n = do
    f <- arbitraryId
    t <- (arbitrarySession (n `div` 4))
    return (f,t)

arbitrarySession :: Int -> Gen Type
arbitrarySession 0 = return (Skip defaultPos)
arbitrarySession n = do
  t <- oneof [
              liftM2 Semi (arbitrarySession (n `div` 4)) (arbitrarySession (n `div` 4))
              ,liftM Choice Out arbitrary
              ,liftM Choice In arbitrary
              ,liftM Choice Out (arbitraryTypeMap (n `div` 4))
              ,liftM Choice In (arbitraryTypeMap (n `div` 4))
              ,liftM2 Rec (arbitraryId) (arbitraryType (n `div` 4))
             ]

  if (isSessionType Map.empty Map.empty t) then
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
  where kEnv = Map.fromList [(mkVar defaultPos "α", kindTL), (mkVar  defaultPos "β", kindTL),
                             (mkVar defaultPos "γ", kindTU), (mkVar  defaultPos "δ", kindTU),
                             (mkVar defaultPos "ρ", kindSL), (mkVar  defaultPos "τ", kindSL),
                             (mkVar defaultPos "φ", kindSU), (mkVar  defaultPos "ψ", kindSU)]
