import           Test.QuickCheck
import           Equivalence.Equivalence
import           Equivalence.Bisimulation
import           Equivalence.Normalisation
import           Validation.Substitution
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

main = verboseCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_subs_kind_preservation1
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_norm_preserves_bisim
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_distribution
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_dual_convolution

-- Convenience

bisim :: Type -> Type -> Bool
bisim = bisimilar Map.empty
equiv :: Type -> Type -> Bool
equiv = equivalent Map.empty Map.empty
norm :: Type -> Type
norm = normalise Map.empty
pos = defaultPos

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))
        -- TODO: This env should only contain the free vars of t; plus
        -- its kind may be SU
        
kinded :: Type -> Bool
kinded = isJust . kindOf

kindOf :: Type -> Maybe Kind
kindOf t
  | null (errors s) = Just k
  | otherwise       = Nothing
  where (k, s) = runState (synthetise kindEnv t) (initialState "Kind syntesis")

-- Bisimilar types are bisimilar
prop_bisimilar :: BisimPair -> Property
prop_bisimilar (BisimPair t u) = kinded t && kinded u ==> t `bisim` u

-- Equivalence
prop_equivalent :: BisimPair -> Property
prop_equivalent (BisimPair t u) = kinded t ==> equiv t u

-- Normalisation preserves bisimilarity
prop_norm_preserves_bisim :: Type -> Property
prop_norm_preserves_bisim t = kinded t' ==> bisim u v
  where t' = renameType t
        [u, v] = renameList [t, normalise Map.empty t']
        -- Normalisation requires a renamed type

-- Duality is a convolution
prop_dual_convolution :: Type -> Property
prop_dual_convolution t = kinded t ==> dual (dual t) == t

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
prop_distribution t = kinded t ==>
  collect (nodes t) $
  tabulate "Type constructors" [constr t] $
  True

-- The number of nodes in a type
nodes :: Type -> Int
nodes (Semi _ t u)   = 1 + nodes t + nodes u
nodes (Choice _ _ m) = 1 + Map.foldr (\t acc -> nodes t + acc) 0 m
nodes (Rec _ _ t)    = 1 + nodes t
-- Skip, Message, TypeVar
nodes _              = 1

-- The constructor of a type
constr :: Type -> String
constr (Basic _ _) = "Basic"
constr (Syntax.Types.Fun _ _ _ _) = "Fun"
constr (PairType _ _ _) = "PairType"
constr (Datatype _ _) = "Datatype"
constr (Skip _) = "Skip"
constr (Semi _ _ _) = "Semi"
constr (Message _ _ _) = "Message"
constr (Choice _ _ _) = "Choice"
constr (Rec _ _ _) = "Rec"
constr (TypeVar _ _) = "TypeVar"
constr (TypeName _ _) = "TypeName"
constr (Dualof _ _) = "Dualof"

-- Arbitrary

instance Arbitrary Multiplicity where
  arbitrary = elements [Un, Lin]

instance Arbitrary Polarity where
  arbitrary = elements [In, Out]

ids :: [String]            -- Type Variables
ids = ["x", "y", "z"]

freeTypeVar :: TypeVar
freeTypeVar = mkVar pos "d"

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
  arbitrary = liftM3 TypeVarBind (return pos) arbitrary arbitrary

instance Arbitrary BasicType where
  arbitrary = elements [IntType, CharType, BoolType, UnitType]
{-
instance Arbitrary Type where
  arbitrary = sized arbitrarySession -- Warning: not renamed

arbitrarySession :: Int -> Gen Type
arbitrarySession 0 = oneof
  [ return $ Skip pos
  , liftM3 Message arbitrary arbitrary arbitrary
  ]
arbitrarySession n = oneof
  [ liftM3 Semi arbitrary (arbitrarySession (n `div` 4)) (arbitrarySession (n `div` 4))
  , liftM3 Choice arbitrary arbitrary (arbitraryTypeMap (n `div` 4))
  , liftM2 TypeVar arbitrary arbitrary
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
-}

-- Arbitrary pairs of bisimilar types

data BisimPair = BisimPair Type Type

instance Show BisimPair where
  show (BisimPair t u) = show t ++ " bisimilar-to " ++ show u

instance Arbitrary BisimPair where
  arbitrary = do
    (t, u) <- bisimPair
    let [t', u'] = renameList [t, u]
    return $ BisimPair t' u'

bisimPair :: Gen (Type, Type)
bisimPair =
  oneof
    [ skipPair
    , semiPair
    , messagePair
    -- , choicePair
    , recPair
    , varPair
    -- Lemma 3.4 _ Laws for sequential composition (ICFP'16)
    , skipt
    , tskip
    -- , assoc
    -- , distrib
    -- Lemma 3.5 _ Laws for mu-types (ICFP'16)
    , recrec
    , recFree
    , alphaConvert
    , subsOnBoth
    , unfoldt
    -- Commutativity
    , commut
    ]

-- The various session type constructors

skipPair :: Gen (Type, Type)
skipPair = return (Skip pos, Skip pos)

semiPair :: Gen (Type, Type)
semiPair = do
  (t, u) <- bisimPair
  (v, w) <- bisimPair
  return (Semi pos t v, Semi pos u w)

messagePair :: Gen (Type, Type)
messagePair = do
  (p, b) <- arbitrary
  return (Message pos p b, Message pos p b)

-- choicePair :: Gen (Type, Type)
-- choicePair = do
--   p <- arbitrary
--   m <- arbitraryTypeMap
--   return (Choice pos p m, Choice pos p m) -- TODO: use two equivalent maps

recPair :: Gen (Type, Type)
recPair = do
  (t, u) <- bisimPair
  xk <- arbitrary
  return (Rec pos xk t, Rec pos xk u)

varPair :: Gen (Type, Type)
varPair = do
  x <- arbitrary
  return (TypeVar pos x, TypeVar pos x)

-- Lemma 3.4 _ Laws for sequential composition (ICFP'16)

skipt :: Gen (Type, Type)
skipt = do
  (t, u) <- bisimPair
  return (Semi pos (Skip pos) t, u)

tskip :: Gen (Type, Type)
tskip = do
  (t, u) <- bisimPair
  return (Semi pos t (Skip pos), u)

assoc :: Gen (Type, Type)
assoc = do
  (t, u) <- bisimPair
  (v, w) <- bisimPair
  (x, y) <- bisimPair
  return (Semi pos t (Semi pos v x),
          Semi pos (Semi pos u w) y)
{-
distrib :: GenBisimPair
distrib t u = do
  p <- arbitrary
  m <- sized arbitraryTypeMap
  return (Semi pos (Choice pos p m) t,
          Choice pos p (Map.map (\v -> Semi pos v u) m))
-}
-- Lemma 3.5 _ Laws for mu-types (ICFP'16)

recrec :: Gen (Type, Type)
recrec = do
  (t, u) <- bisimPair
  (xk@(TypeVarBind _ x _), yk@(TypeVarBind _ y _)) <- arbitrary
  return (Rec pos xk (Rec pos yk t),
          Rec pos xk (subs (TypeVar pos x) y u))
          -- Rec pos xk (subs (TypeVar pos x) y (renameType u)))

recFree :: Gen (Type, Type)
recFree = do
  (t, u) <- bisimPair
  k <- arbitrary
  return (Rec pos (TypeVarBind pos freeTypeVar k) t, u)
  -- Note: the rec-var must be distinct from the free variables of t

alphaConvert :: Gen (Type, Type) -- (fixed wrt to ICFP'16)
alphaConvert = do
  (t, u) <- bisimPair
  (x, k) <- arbitrary
  let y = freeTypeVar
  return (Rec pos (TypeVarBind pos x k) t,
          Rec pos (TypeVarBind pos y k) (subs (TypeVar pos y) x u))

subsOnBoth :: Gen (Type, Type)
subsOnBoth = do
  (t, u) <- bisimPair
  (v, w) <- bisimPair
  x <- arbitrary
  return (subs t x v,
          subs u x w)

unfoldt :: Gen (Type, Type)
unfoldt = do
  (t, u) <- bisimPair
  xk <- arbitrary
  return (Rec pos xk t,
          unfold (Rec pos xk u))
          -- unfold (Rec pos xk (renameType u)))

-- Commutativity

commut :: Gen (Type, Type)
commut = do
  (t, u) <- bisimPair
  return (u, t)

