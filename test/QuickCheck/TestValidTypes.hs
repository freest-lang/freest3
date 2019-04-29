import           Test.QuickCheck
import           Equivalence.Bisimulation
import           Equivalence.Equivalence
import           Equivalence.Normalisation
import           Validation.Rename
import           Validation.Kinding
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import           Utils.FreestState
import           Control.Monad.State
import           Control.Monad
import           Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Debug.Trace

-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_self_bisimilar
main = quickCheckWith stdArgs {maxSuccess = 10000} prop_subs_kind_preservation1
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_kinded
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_norm_equiv
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_dual
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_same_equivs

-- Convenience

bisim = bisimilar Map.empty
equiv :: Type -> Type -> Bool
equiv = equivalent Map.empty Map.empty
norm :: Type -> Type
norm = normalise Map.empty
pos = defaultPos

kinded :: Type -> Bool
kinded t = null (errors state)
  where
  state = execState (synthetise kindEnv t) (initialState "Quick Checking")
  kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))
        -- This env contains the free vars only for bound vars should
        -- have been renamed
  
-- Bisimilarity

prop_bisimilar :: BisimPair -> Property
prop_bisimilar (BisimPair t u) = kinded t ==>
  evalState (return $ bisim t u) ()

prop_bisimilar_trace :: BisimPair -> Property
prop_bisimilar_trace (BisimPair t u) = kinded t ==>
  evalState (trace (show t ++ " bisim " ++ show u) (return $ bisim t u)) ()

prop_self_bisimilar :: Type -> Property
prop_self_bisimilar t = kinded t ==>
  evalState (trace (show u ++ " self-bisimilar-to " ++ show v) (return $ bisim u v)) ()
  where [u, v] = renameList [t, t]

-- Kinding -- Test cases are not necessarily kinded

-- prop_kinded :: Type -> Property
-- prop_kinded t = contr t ==> null (errors state)
--   where state = execState (synthetise kindEnv t) (initialState "Quick Checking")
--         kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))

kindOf :: Type -> Maybe Kind
kindOf t
  | null (errors s) = Just k
  | otherwise       = Nothing
  where (k, s) = runState (synthetise kindEnv t) (initialState "Kind syntesis")
        kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))
        -- This env should only contain the free vars of t; its kind may be SU

-- Normalisation

prop_norm_equiv :: Type -> Property
prop_norm_equiv t = kinded t ==>
  evalState (trace (show u ++ " equiv-to-normalised " ++ show v) (return $ bisim u v)) ()
  where [u, v] = renameList [t, normalise Map.empty t]

-- Duality

prop_dual :: Type -> Property
prop_dual t = kinded t ==> dual (dual t) == t

-- Lemma 3.1(ii) _ Substitution and kind preservation (ICFP'16)
prop_subs_kind_preservation2 :: TypeVar -> Kind -> Type -> Property
prop_subs_kind_preservation2 x k t =
  isJust k1 ==> k1 == kindOf (unfold u)
  where u = renameType $ Rec pos (TypeVarBind pos x k) t
        k1 = kindOf u

-- Lemma 3.3 _ Laws for terminated communication (ICFP'16)
prop_terminated1 :: Type -> Type -> Property
prop_terminated1 t u =
  kinded t ==> not (terminated t && terminated u) || bisim t u

-- Laws for terminated communication (bonus)
prop_terminated2 :: Type -> Property
prop_terminated2 t =
  kinded t' ==> terminated t' == bisim t' (Skip pos)
  where t' = renameType t

-- Distribution

prop_distribution :: Type -> Property
prop_distribution t = kinded t ==> collect (nodes t) True

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

ids :: [String]            -- Type Variables
ids = ["x", "y", "z"]

choices :: [String]        -- Program Variables
choices = ["A", "B", "C"]

instance Arbitrary TypeVar where
  arbitrary = arbitraryVar ids

instance Arbitrary ProgVar where
  arbitrary = arbitraryVar choices

arbitraryVar :: Variable b => [String] -> Gen b
arbitraryVar ids = do
  id <- elements ids
  return $ mkVar pos id

instance Arbitrary Kind where
  arbitrary = elements [kindSL pos, kindSU pos] -- Session types only

instance Arbitrary TypeVarBind where
  arbitrary = liftM3 TypeVarBind arbitrary arbitrary arbitrary

instance Arbitrary BasicType where
  arbitrary = elements [IntType, CharType, BoolType, UnitType]

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
  , liftM3 Rec arbitrary arbitrary (arbitrarySession (n `div` 4))
  ]

arbitraryTypeMap :: Int -> Gen TypeMap
arbitraryTypeMap n = do
  k <- choose (1, length choices)
  m <- vectorOf k $ arbitraryField (n `div` 4)
  return $ Map.fromList m

arbitraryField :: Int -> Gen (ProgVar, Type)
arbitraryField n = do
    x <- arbitrary
    t <- arbitrarySession (n `div` 4)
    return (x, t)

-- Arbitrary Pairs of Bisimilar Types

data BisimPair = BisimPair Type Type

instance Show BisimPair where
  show (BisimPair t u) = show t ++ " bisimilar-to " ++ show u

instance Arbitrary BisimPair where
  arbitrary = do
    (t, u) <- oneof
      [
      -- Lemma 3.4 _ Laws for sequential compositions (ICFP'16)
        skipt
      , tskip
      , assoc
      , distrib      
      -- Lemma 3.5 _ Laws for mu-types (ICFP'16)
      , recrec
      , recFree
      , alphaConvert
      , subsOnBoth
      , unfoldt
      -- Self
      , self
      ]
    let [t', u'] = renameList [t, u]
    return $ BisimPair t' u'

-- The various axioms

-- Lemma 3.4 _ Laws for sequential compositions (ICFP'16)

skipt :: Gen (Type, Type)
skipt = do
  t <- arbitrary
  return (Semi pos (Skip pos) t, t)

tskip :: Gen (Type, Type)
tskip = do
  t <- arbitrary
  return (Semi pos t (Skip pos), t)

assoc :: Gen (Type, Type)
assoc = do
  (t, u, v) <- arbitrary
  return (Semi pos t (Semi pos u v), Semi pos (Semi pos t u) v)

distrib :: Gen (Type, Type)
distrib = do
  (t, p) <- arbitrary
  m <- sized arbitraryTypeMap
  return (Semi pos (Choice pos p m) t, Choice pos p (Map.map (\u -> Semi pos u t) m))

-- Lemma 3.5 _ Laws for mu-types (ICFP'16)

recrec :: Gen (Type, Type)
recrec = do
  (xk@(TypeVarBind _ x _), yk@(TypeVarBind _ y _), t) <- arbitrary
  return (Rec pos xk (Rec pos yk t), Rec pos xk (subs (TypeVar pos x) y (renameType t)))

recFree :: Gen (Type, Type)
recFree = do
  t <- arbitrary
  return (Rec pos (TypeVarBind pos (mkVar pos "∂") (kindSL pos)) t, t)
  -- Note: the rec-var must be distinct from the variables on t

alphaConvert ::  Gen (Type, Type)
alphaConvert = do
  (t, xk@(TypeVarBind _ x _), y) <-arbitrary
  return (Rec pos xk t, subs (TypeVar pos y) x (renameType t))

subsOnBoth :: Gen (Type, Type)
subsOnBoth = do
  (BisimPair t u, v, x) <- arbitrary
  return (subs t x v, subs u x v)

unfoldt :: Gen (Type, Type)
unfoldt = do
  (xk, t) <- arbitrary
  let u = Rec pos xk t
  return (u, unfold (renameType u))

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
