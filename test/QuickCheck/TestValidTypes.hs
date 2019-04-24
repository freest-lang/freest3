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

-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_self_bisimilar
main = quickCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 1000} prop_kinded
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
prop_bisimilar (BisimPair t u) = contr t ==>
  evalState (trace (show t ++ " bisim " ++ show u) (return $ bisim t u)) ()

prop_self_bisimilar :: Type -> Property
prop_self_bisimilar t = contr t ==>
  evalState (trace (show u ++ " self-bisimilar-to " ++ show v) (return $ bisim u v)) ()
  where [u, v] = renameList [t, t]

prop_equivalent :: BisimPair -> Property
prop_equivalent (BisimPair t u) = contr t ==> equiv t u

prop_kinded :: Type -> Property
prop_kinded t = contr t ==> wellFormed t

wellFormed :: Type -> Bool
wellFormed t = null (errors state)
  where state = execState (synthetise kindEnv t) (initialState "Quick Checking")
        kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos))) -- TODO: only the free vars should go into this environment
  
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
  arbitrary = sized arbitrarySession -- Not renamed

arbitrarySession :: Int -> Gen Type
arbitrarySession 0 = oneof
  [ liftM Skip arbitrary
  , liftM2 TypeVar arbitrary arbitrary
  ]
arbitrarySession n = oneof
  [ liftM3 Semi arbitrary (arbitrarySession (n `div` 4)) (arbitrarySession (n `div` 4))
  , liftM3 Message arbitrary arbitrary arbitrary
  , liftM3 Choice arbitrary arbitrary (arbitraryTypeMap (n `div` 4))
--  , liftM3 Rec arbitrary arbitrary (arbitrarySession (n `div` 4))
  ]

-- instance Arbitrary TypeMap where
--   arbitrary = return Map.empty

-- TODO: TypeMap is already an instance of Arbitrary. Use it! In sized mode
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
  arbitrary = do
    (t, u) <- oneof
      [ -- assoc
      -- , distrib
      -- unfoldt,
      -- recrec
      -- skipt
      -- tskip
      -- subsOnBoth
        -- self
        voidRec
      ]
    let [t', u'] = renameList [t, u]
    return $ BisimPair t' u'

-- The various axioms

assoc :: Gen (Type, Type)
assoc = do
  (t, u, v) <- arbitrary
  return (Semi pos t (Semi pos u v), Semi pos (Semi pos t u) v)

distrib :: Gen (Type, Type)
distrib = do
  (t, p) <- arbitrary
  m <- sized arbitraryTypeMap
  return (Semi pos (Choice pos p m) t, Choice pos p (Map.map (\u -> Semi pos u t) m))

unfoldt :: Gen (Type, Type)
unfoldt = do
  (xk, t) <- arbitrary
  let u = Rec pos xk t
  return (u, unfold u)

recrec :: Gen (Type, Type)
recrec = do
  (xk@(TypeVarBind _ x _), yk@(TypeVarBind _ y _), t) <- arbitrary
  return (Rec pos xk (Rec pos yk t), Rec pos xk (subs (TypeVar pos x) y t))

voidRec :: Gen (Type, Type)
voidRec = do
  t <- arbitrary
  return (Rec pos (TypeVarBind pos (mkVar pos "∂") (kindSL pos)) t, t)
  -- Note: the rec-var must be distinct from the variables on t

skipt :: Gen (Type, Type)
skipt = do
  t <- arbitrary
  return (Semi pos (Skip pos) t, t)

tskip :: Gen (Type, Type)
tskip = do
  t <- arbitrary
  return (Semi pos t (Skip pos), t)

{-
-- TODO: to be used in sized mode
subsOnBoth :: Gen (Type, Type)
subsOnBoth = do
  (BisimPair t u, v, x) <- arbitrary
  return (subs t x v, subs u x v)
-}

self :: Gen (Type, Type)
self = do
  t <- arbitrary
  return (t, t)
  
-- Renaming

renameType t = evalState (rename Map.empty t) (initialState "Renaming for QuickCheck")

renameList ts = evalState (mapM (rename Map.empty) ts) (initialState "Renaming for QuickCheck")
