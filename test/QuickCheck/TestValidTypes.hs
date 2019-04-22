{-# LANGUAGE TypeSynonymInstances #-}

import           Test.QuickCheck
import           Equivalence.Bisimulation
import           Equivalence.Equivalence
import           Equivalence.Normalisation
import           Validation.Rename
import           Validation.Kinding
import           Validation.Contractive
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import           Utils.FreestState
import           Control.Monad.State
import           Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Debug.Trace

-- main = quickCheckWith stdArgs {maxSuccess = 1000} prop_kinded
main = quickCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_equivalent
-- main = quickCheckWith stdArgs {maxSuccess = 1000} prop_eq_type
-- main = quickCheckWith stdArgs {maxSuccess = 1000} prop_normal_eq_type
-- main = quickCheckWith stdArgs {maxSuccess = 1000} prop_same_equivs

-- Convenience

bisim = bisimilar Map.empty
equiv = equivalent Map.empty Map.empty
contr = contractive Map.empty Map.empty
norm = normalise Map.empty
pos = defaultPos

-- Properties

prop_bisimilar :: BisimPair -> Property
prop_bisimilar (BisimPair t u) = contr t ==> bisimi t u

bisimi t u = evalState (trace (show t ++ " bisim " ++ show u) (return $ bisim t u)) ()

prop_self_bisimilar :: Type -> Property
prop_self_bisimilar t = contr t ==> self_bisim t

self_bisim t = evalState (trace (show t) (return $ bisim t t)) ()

prop_equivalent :: BisimPair -> Property
prop_equivalent (BisimPair t u) = contr t ==> equiv t u

prop_kinded :: Type -> Property
prop_kinded t = contr t ==> wellFormed t

wellFormed :: Type -> Bool
wellFormed t = null (errors state)
  where state = execState (synthetise kindEnv t) (initialState "Quick Checking")
        kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos))) -- TODO: only the free vars should go into this environment
  
prop_same_bisims :: Type -> Property
prop_same_bisims t = contr t ==> bisim t t

prop_eq_type :: Type -> Property
prop_eq_type t = contr t ==> t == t

prop_normal_eq_type :: Type -> Property
prop_normal_eq_type t = contr t ==> norm t == norm t

prop_dual :: Type -> Bool
prop_dual t = dual (dual t) == t

prop_distribution :: Type -> Property
prop_distribution d = collect (nodes d) True

-- The number of nodes in a type
nodes :: Type -> Int
nodes (Semi _ t u)   = 1 + nodes t + nodes u
nodes (Choice _ _ m) = 1 + Map.foldr (\t acc -> nodes t + acc) 0 m
nodes (Rec _ _ t)    = 1 + nodes t
-- Skip, Message, TypeVar
nodes _              = 1

-- prop_show :: Type -> Bool
-- prop_show t = show (read (show t) :: Type) == show t

-- Arbitrary Types

instance Arbitrary Multiplicity where
  arbitrary = elements [Un, Lin]

instance Arbitrary Polarity where
  arbitrary = elements [In, Out]

instance Arbitrary Pos where
  arbitrary = return pos

ids :: [String]      -- Type Variables
ids = ["x", "y", "z"]

instance Arbitrary TypeVar where
  arbitrary = arbitraryVar ids

choices :: [String]        -- Program Variables
choices = ["A", "B", "C"]

instance Arbitrary ProgVar where
  arbitrary = arbitraryVar choices

arbitraryVar :: Variable b => [String] -> Gen b
arbitraryVar ids = do
  id <- elements ids
  return $ mkVar pos id

instance Arbitrary Kind where
  arbitrary = return $ kindSL pos

instance Arbitrary TypeVarBind where
  arbitrary = liftM3 TypeVarBind arbitrary arbitrary arbitrary

instance Arbitrary BasicType where
  arbitrary = elements [IntType, CharType, BoolType{-, UnitType-}]

instance Arbitrary Type where
  arbitrary = sized arbitrarySession

arbitrarySession :: Int -> Gen Type
arbitrarySession 0 = oneof
  [ liftM Skip arbitrary
  , liftM2 TypeVar arbitrary arbitrary
  ]
arbitrarySession n = oneof
  [ liftM3 Semi arbitrary (arbitrarySession (n `div` 4)) (arbitrarySession (n `div` 4))
  , liftM3 Message arbitrary arbitrary arbitrary
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

-- Arbitrary Pairs of Bisimilar Types

data BisimPair = BisimPair Type Type

instance Show BisimPair where
  show (BisimPair t u) = show t ++ " bisimilar-to " ++ show u

instance Arbitrary BisimPair where
  arbitrary = oneof
    [ assoc
    ]

assoc :: Gen BisimPair
assoc = do
  (t, u, v) <- arbitrary
  let [a,b,c,d,e,f] = renameList [t,u,v,t,u,v]
  -- let (PairType _ t' (PairType pos u' v')) = evalState (rename Map.empty (PairType pos t (PairType pos u v))) (initialState "Renaming for QuickCheck")
  return $ BisimPair (Semi pos a (Semi pos b c))
                     (Semi pos (Semi pos d e) f)
                     
renameList ts =
  evalState (mapM (rename Map.empty) ts) (initialState "Renaming for QuickCheck")

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
arbitrarySession 0 = return (Skip pos)
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
  where kEnv = Map.fromList [(mkVar pos "α", kindTL), (mkVar pos "β", kindTL),
                             (mkVar pos "x", kindTL), (mkVar pos "y", kindTL),
                             (mkVar pos "z", kindTL),
                             (mkVar pos "γ", kindTU), (mkVar pos "δ", kindTU),
                             (mkVar pos "ρ", kindSL), (mkVar pos "τ", kindSL),
                             (mkVar pos "φ", kindSU), (mkVar pos "ψ", kindSU)]

-}
