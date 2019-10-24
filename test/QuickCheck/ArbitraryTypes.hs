module QuickCheck.ArbitraryTypes
( BisimPair(..)
, NonBisimPair(..)
, ids
) where

import           Test.QuickCheck
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Syntax.Show
import qualified Validation.Rename as Rename
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
    let [t', u'] = Rename.renameTypes [t, u]
    return $ BisimPair t' u'
    
instance Arbitrary Type where
  arbitrary = do
    (BisimPair _ t) <- arbitrary
    return t

type PairGen = Int -> Gen (Type, Type)

bisimPair :: PairGen
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
    [ skipPair
    , messagePair
    , varPair
    , choicePair bisimPair n
    , recPair bisimPair n
    , semiPair bisimPair n
    -- Lemma 3.4 _ Laws for sequential composition (ICFP'16)
    , skipT n
    , tSkip n
    , distrib n
    , assoc n
    , commut n
    -- Lemma 3.5 _ Laws for mu-types (ICFP'16)
    , recRecL n
    , recRecR n
    , recFree n
    -- , alphaConvert n
    , subsOnBoth n
    , unfoldt n
    ]

-- The various session type constructors

skipPair :: Gen (Type, Type)
skipPair = return (Skip pos,
                   Skip pos)

messagePair :: Gen (Type, Type)
messagePair = do
  (p, b) <- arbitrary
  return (Message pos p b,
          Message pos p b)

varPair :: Gen (Type, Type)
varPair = do
  x <- arbitrary
  return (TypeVar pos x,
          TypeVar pos x)

semiPair :: PairGen -> Int -> Gen (Type, Type)
semiPair pairGen n = do
  (t, u) <- pairGen (n `div` 8)
  (v, w) <- pairGen (n `div` 8)
  return (Semi pos t v,
          Semi pos u w)

choicePair :: PairGen -> Int -> Gen (Type, Type)
choicePair pairGen n = do
  p <- arbitrary
  (m1, m2) <- typeMapPair pairGen n
  return (Choice pos p m1,
          Choice pos p m2)

typeMapPair :: PairGen -> Int -> Gen (TypeMap, TypeMap)
typeMapPair pairGen n = do
  k <- choose (1, length choices)
  pairs <- vectorOf k $ fieldPair (n `div` k)
  let (f1, f2) = unzip pairs
  return (Map.fromList f1, Map.fromList f2)
  where
  fieldPair :: Int -> Gen ((ProgVar, Type), (ProgVar, Type))
  fieldPair n = do
    (t, u) <- pairGen (n `div` 4)
    x <- arbitrary
    return ((x, t), (x, u))

recPair :: PairGen -> Int -> Gen (Type, Type)
recPair pairGen n = do
  (t, u) <- pairGen (n `div` 4)
  xk <- arbitrary
  return (Rec pos xk t, Rec pos xk u)

-- Lemma 3.4 _ Laws for sequential composition (ICFP'16)

skipT :: Int -> Gen (Type, Type)
skipT n = do
  (t, u) <- bisimPair (n `div` 2)
  return (Semi pos (Skip pos) t, u)

tSkip :: Int -> Gen (Type, Type)
tSkip n = do
  (t, u) <- bisimPair (n `div` 2)
  return (Semi pos t (Skip pos), u)

distrib :: Int -> Gen (Type, Type)
distrib n = do
  (t, u) <- bisimPair (n `div` 4)
  (m1, m2) <- typeMapPair bisimPair (n `div` 4)
  p <- arbitrary
  return (Semi pos (Choice pos p m1) t,
          Choice pos p (Map.map (\v -> Semi pos v u) m2))

assoc :: Int -> Gen (Type, Type)
assoc n = do
  (t, u) <- bisimPair (n `div` 6)
  (v, w) <- bisimPair (n `div` 6)
  (x, y) <- bisimPair (n `div` 6)
  return (Semi pos t (Semi pos v x),
          Semi pos (Semi pos u w) y)
          
commut :: Int -> Gen (Type, Type)  -- TODO: this axiom be distributed among all others
commut n = do
  (t, u) <- bisimPair (n `div` 4)
  return (u, t)

-- Lemma 3.5 _ Laws for mu-types (ICFP'16)

recRecL :: Int -> Gen (Type, Type)
recRecL n = do
  (t, u) <- bisimPair (n `div` 2)
  xk@(TypeVarBind _ x _) <- arbitrary
  yk@(TypeVarBind _ y _) <- arbitrary
  let u' = Rename.renameType u -- this type will be in a substitution
  return (Rec pos xk (Rec pos yk t),
          Rec pos xk (Rename.subs (TypeVar pos x) y u'))

recRecR :: Int -> Gen (Type, Type)
recRecR n = do
  (t, u) <- bisimPair (n `div` 2)
  xk@(TypeVarBind _ x _) <- arbitrary
  yk@(TypeVarBind _ y _) <- arbitrary
  let u' = Rename.renameType u -- this type will be in a substitution
  return (Rec pos xk (Rec pos yk t),
          Rec pos yk (Rename.subs (TypeVar pos y) x u'))

recFree :: Int -> Gen (Type, Type)
recFree n = do
  (t, u) <- bisimPair (n `div` 2)
  k <- arbitrary
  return (Rec pos (TypeVarBind pos freeTypeVar k) t, u)

-- alphaConvert :: Int -> Gen (Type, Type) -- (fixed wrt to ICFP'16)
-- alphaConvert n = do
--   (t, u) <- bisimPair n
--   (x, k) <- arbitrary
--   let y = freeTypeVar -- TODO: Here we need a genunine free var
--   return (Rec pos (TypeVarBind pos x k) t,
--           Rec pos (TypeVarBind pos y k) (Rename.subs (TypeVar pos y) x u))

subsOnBoth :: Int -> Gen (Type, Type)
subsOnBoth n = do
  (t, u) <- bisimPair (n `div` 4)
  (v, w) <- bisimPair (n `div` 4)
  let [t',u',v',w'] = Rename.renameTypes [t,u,v,w] -- these types will be in a substitution
  x <- arbitrary
  return (Rename.subs t' x v',
          Rename.subs u' x w')

unfoldt :: Int -> Gen (Type, Type)
unfoldt n = do
  (t, u) <- bisimPair (n `div` 2)
  let u' = Rename.renameType u -- this type will be unfolded
  xk <- arbitrary
  return (Rec pos xk t,
          Rename.unfold (Rec pos xk u'))

-- Arbitrary pairs of non-bisimilar types

data NonBisimPair = NonBisimPair Type Type

instance Show NonBisimPair where
  show (NonBisimPair t u) = show t ++ " not-bisimilar-to " ++ show u

instance Arbitrary NonBisimPair where
  arbitrary = do
    (t, u) <- sized nonBisimPair
    let [t', u'] = Rename.renameTypes [t, u]
    return $ NonBisimPair t' u'

nonBisimPair :: Int -> Gen (Type, Type)
nonBisimPair 0 =
  oneof
    -- Anti-axioms
    [
      skipMessage
    , varSkip
    , messageVar
    ]
nonBisimPair n =
  oneof
    -- The various type constructors, excluding the "atoms": skip, message, var
    [
      semiPair nonBisimPair n
    , recPair nonBisimPair n
    , choicePair nonBisimPair n
    ]

-- A few anti-axioms

skipMessage :: Gen (Type, Type)
skipMessage = do
  (p, b) <- arbitrary
  return (Skip pos,
          Message pos p b)

varSkip :: Gen (Type, Type)
varSkip = do
  x <- arbitrary
  return (TypeVar pos x,
          Skip pos)

messageVar :: Gen (Type, Type)
messageVar = do
  (p, b, x) <- arbitrary
  return (Message pos p b,
          TypeVar pos x)

messageInOut :: Gen (Type, Type)
messageInOut = do
  (p, b) <- arbitrary
  return (Message pos p b,
          Message pos (dual p) b)
