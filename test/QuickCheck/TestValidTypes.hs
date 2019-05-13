import           Test.QuickCheck
import           Test.QuickCheck.Random
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

main = quickCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 10000, replay = Just (mkQCGen 42, 0)} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_subs_kind_preservation1
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_norm_preserves_bisim
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_distribution
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_dual_convolution

-- Convenience

bisim :: Type -> Type -> Bool
bisim = bisimilar Map.empty
equiv :: Type -> Type -> Bool
equiv = equivalent Map.empty kindEnv
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

prop_distribution :: BisimPair -> Property
prop_distribution (BisimPair t _) = kinded t ==>
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
freeTypeVar = mkVar pos "δ"

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

-- Arbitrary pairs of bisimilar types

data BisimPair = BisimPair Type Type

instance Show BisimPair where
  show (BisimPair t u) = show t ++ " bisimilar-to " ++ show u

instance Arbitrary BisimPair where
  arbitrary = do
    (t, u) <- sized bisimPair
    let [t', u'] = {- renameList -} [t, u]
    return $ BisimPair t' u'

bisimPair :: Int -> Gen (Type, Type)
bisimPair n =
  oneof $
    -- The various type constructors
    [ skipPair
    , messagePair
    , varPair
    ]
    ++ [choicePair n   | n > 0]
    ++ [recPair n      | n > 0]
    ++ [semiPair n     | n > 0]
    -- Lemma 3.4 _ Laws for sequential composition (ICFP'16)
    ++ [skipt n        | n > 0]
    ++ [tskip n        | n > 0]
    ++ [assoc n        | n > 0]
    ++ [distrib n      | n > 0]
    -- Lemma 3.5 _ Laws for mu-types (ICFP'16)
    ++ [recrec n       | n > 0]
    ++ [recFree n      | n > 0]
    -- ++ [alphaConvert n | n > 0]
    ++ [subsOnBoth n   | n > 0]
    ++ [unfoldt n      | n > 0]
    -- Commutativity
    ++ [commut n       | n > 0]

-- The various session type constructors

skipPair :: Gen (Type, Type)
skipPair = return (Skip pos,
                   Skip pos)

semiPair :: Int -> Gen (Type, Type)
semiPair n = do
  (t, u) <- bisimPair (n `div` 2)
  (v, w) <- bisimPair (n `div` 2)
  return (Semi pos t v,
          Semi pos u w)

messagePair :: Gen (Type, Type)
messagePair = do
  (p, b) <- arbitrary
  return (Message pos p b,
          Message pos p b)

choicePair :: Int -> Gen (Type, Type)
choicePair n = do
  p <- arbitrary
  pairs <- fieldPairs n
  let (f1, f2) = unzip pairs
  return (Choice pos p (Map.fromList f1),
          Choice pos p (Map.fromList f2))

fieldPairs :: Int -> Gen [((ProgVar, Type), (ProgVar, Type))]
fieldPairs n = do
  k <- choose (1, length choices)
  vectorOf k $ field (n `div` k)
  where
  field :: Int -> Gen ((ProgVar, Type), (ProgVar, Type))
  field n = do
      (t, u) <- bisimPair n
      x <- arbitrary
      return ((x, t), (x, u))

recPair :: Int -> Gen (Type, Type)
recPair n = do
  (t, u) <- bisimPair n
  xk <- arbitrary
  return (Rec pos xk t, Rec pos xk u)

varPair :: Gen (Type, Type)
varPair = do
  x <- arbitrary
  return (TypeVar pos x, TypeVar pos x)

-- Lemma 3.4 _ Laws for sequential composition (ICFP'16)

skipt :: Int -> Gen (Type, Type)
skipt n = do
  (t, u) <- bisimPair n
  return (Semi pos (Skip pos) t, u)

tskip :: Int -> Gen (Type, Type)
tskip n = do
  (t, u) <- bisimPair n
  return (Semi pos t (Skip pos), u)

assoc :: Int -> Gen (Type, Type)
assoc n = do
  (t, u) <- bisimPair (n `div` 3)
  (v, w) <- bisimPair (n `div` 3)
  (x, y) <- bisimPair (n `div` 3)
  return (Semi pos t (Semi pos v x),
          Semi pos (Semi pos u w) y)

distrib :: Int -> Gen (Type, Type)
distrib n = do
  (t, u) <- bisimPair (n `div` 2)
  pairs <- fieldPairs (n `div` 2)
  p <- arbitrary
  let (f1, f2) = unzip pairs
  return (Semi pos (Choice pos p (Map.fromList f1)) t,
          Choice pos p (Map.map (\v -> Semi pos v u) (Map.fromList f2)))
          -- Choice pos p (Map.fromList (map (\(x,v) -> (x, Semi pos v u)) f2))) 
-- Lemma 3.5 _ Laws for mu-types (ICFP'16)

recrec :: Int -> Gen (Type, Type)
recrec n = do
  (t, u) <- bisimPair n
  (xk@(TypeVarBind _ x _), yk@(TypeVarBind _ y _)) <- arbitrary
  return (Rec pos xk (Rec pos yk t),
          Rec pos xk (subs (TypeVar pos x) y u))

recFree :: Int -> Gen (Type, Type)
recFree n = do
  (t, u) <- bisimPair n
  k <- arbitrary
  return (Rec pos (TypeVarBind pos freeTypeVar k) t, u)
  -- Note: the rec-var must be distinct from the free variables of t

-- alphaConvert :: Int -> Gen (Type, Type) -- (fixed wrt to ICFP'16)
-- alphaConvert n = do
--   (t, u) <- bisimPair n
--   (x, k) <- arbitrary
--   let y = freeTypeVar -- TODO: Here we need a genunine free var
--   return (Rec pos (TypeVarBind pos x k) t,
--           Rec pos (TypeVarBind pos y k) (subs (TypeVar pos y) x u))

subsOnBoth :: Int -> Gen (Type, Type)
subsOnBoth n = do
  (t, u) <- bisimPair (n `div` 2)
  (v, w) <- bisimPair (n `div` 2)
  x <- arbitrary
  return (subs t x v,
          subs u x w)

unfoldt :: Int -> Gen (Type, Type)
unfoldt n = do
  (t, u) <- bisimPair n
  xk <- arbitrary
  return (Rec pos xk t,
          unfold (Rec pos xk u))

-- -- Commutativity

commut :: Int -> Gen (Type, Type)
commut n = do
  (t, u) <- bisimPair n
  return (u, t)

