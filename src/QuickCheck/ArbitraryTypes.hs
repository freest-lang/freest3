module QuickCheck.ArbitraryTypes
( BisimPair(..)
, ids
) where

import           Test.QuickCheck
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Syntax.Show
import           Validation.Substitution
import qualified Data.Map.Strict as Map
import           Control.Monad

pos :: Pos
pos = defaultPos

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
bisimPair 0 =
  oneof
    -- The various type constructors
    [ skipPair
    , messagePair
    , varPair
    ]
bisimPair n =
  oneof
    -- The various type constructors
    [ choicePair n
    , recPair n
    , semiPair n
    -- Lemma 3.4 _ Laws for sequential composition (ICFP'16)
    , skipT n
    , tSkip n
    , assoc n
    , distrib n
    -- Lemma 3.5 _ Laws for mu-types (ICFP'16)
    , recRecL n
    , recRecR n
    , recFree n
    -- , alphaConvert n
    , subsOnBoth n
    , unfoldt n
    -- Commutativity
    , commut n
    ]
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

skipT :: Int -> Gen (Type, Type)
skipT n = do
  (t, u) <- bisimPair n
  return (Semi pos (Skip pos) t, u)

tSkip :: Int -> Gen (Type, Type)
tSkip n = do
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

recRecL :: Int -> Gen (Type, Type)
recRecL n = do
  (t, u) <- bisimPair n
  (xk@(TypeVarBind _ x _), yk@(TypeVarBind _ y _)) <- arbitrary
  return (Rec pos xk (Rec pos yk t),
          Rec pos xk (subs (TypeVar pos x) y u))

recRecR :: Int -> Gen (Type, Type)
recRecR n = do
  (t, u) <- bisimPair n
  (xk@(TypeVarBind _ x _), yk@(TypeVarBind _ y _)) <- arbitrary
  return (Rec pos xk (Rec pos yk t),
          Rec pos yk (subs (TypeVar pos y) x u))

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
