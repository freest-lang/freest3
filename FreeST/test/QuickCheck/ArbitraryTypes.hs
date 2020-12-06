module ArbitraryTypes
  ( BisimPair(..)
  , NonBisimPair(..)
  , ids
  )
where

import           Test.QuickCheck
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
import           Syntax.TypeVariable
import           Syntax.ProgramVariables
import           Syntax.Base             hiding ( pos )
-- import           Parse.Unparser
import qualified Validation.Rename             as Rename
import qualified Data.Map.Strict               as Map
import           Control.Monad

pos :: Pos
pos = defaultPos

instance Arbitrary Multiplicity where
  arbitrary = elements [Un, Lin]

instance Arbitrary T.Polarity where
  arbitrary = elements [T.In, T.Out]

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

instance Arbitrary K.Kind where
  arbitrary = elements [K.kindSL pos, K.kindSU pos] -- Session types only

instance Arbitrary K.KindBind where
  arbitrary = liftM3 K.KindBind (return pos) arbitrary arbitrary

-- instance Arbitrary Type where
--   arbitrary = elements [IntType, CharType, BoolType, UnitType]

-- Arbitrary pairs of bisimilar types

data BisimPair = BisimPair T.Type T.Type

instance Show BisimPair where
  show (BisimPair t u) = show t ++ " bisimilar-to " ++ show u

instance Arbitrary BisimPair where
  arbitrary = do
    (t, u) <- sized bisimPair
    let [t', u'] = Rename.renameTypes [t, u]
    return $ BisimPair t' u'

instance Arbitrary T.Type where
  arbitrary = do
    (BisimPair _ t) <- arbitrary
    return t

type PairGen = Int -> Gen (T.Type, T.Type)

bisimPair :: PairGen
bisimPair 0 =
  oneof
    -- The various type constructors
        [skipPair, messagePair, varPair]
bisimPair n = oneof
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

skipPair :: Gen (T.Type, T.Type)
skipPair = return (T.Skip pos, T.Skip pos)

messagePair :: Gen (T.Type, T.Type)
messagePair = do
  pol <- arbitrary
  t <- elements [T.IntType pos, T.CharType pos, T.BoolType pos, T.UnitType pos]
  return (T.Message pos pol t, T.Message pos pol t)

varPair :: Gen (T.Type, T.Type)
varPair = do
  a <- arbitrary
  return (T.TypeVar pos a, T.TypeVar pos a)

semiPair :: PairGen -> Int -> Gen (T.Type, T.Type)
semiPair pairGen n = do
  (t, u) <- pairGen (n `div` 8)
  (v, w) <- pairGen (n `div` 8)
  return (T.Semi pos t v, T.Semi pos u w)

choicePair :: PairGen -> Int -> Gen (T.Type, T.Type)
choicePair pairGen n = do
  p        <- arbitrary
  (m1, m2) <- typeMapPair pairGen n
  return (T.Choice pos p m1, T.Choice pos p m2)

typeMapPair :: PairGen -> Int -> Gen (T.TypeMap, T.TypeMap)
typeMapPair pairGen n = do
  k     <- choose (1, length choices)
  pairs <- vectorOf k $ fieldPair (n `div` k)
  let (f1, f2) = unzip pairs
  return (Map.fromList f1, Map.fromList f2)
 where
  fieldPair :: Int -> Gen ((ProgVar, T.Type), (ProgVar, T.Type))
  fieldPair n = do
    (t, u) <- pairGen (n `div` 4)
    x      <- arbitrary
    return ((x, t), (x, u))

recPair :: PairGen -> Int -> Gen (T.Type, T.Type)
recPair pairGen n = do
  (t, u) <- pairGen (n `div` 4)
  xk     <- arbitrary
  return (T.Rec pos xk t, T.Rec pos xk u)

-- Lemma 3.4 _ Laws for sequential composition (ICFP'16)

skipT :: Int -> Gen (T.Type, T.Type)
skipT n = do
  (t, u) <- bisimPair (n `div` 2)
  return (T.Semi pos (T.Skip pos) t, u)

tSkip :: Int -> Gen (T.Type, T.Type)
tSkip n = do
  (t, u) <- bisimPair (n `div` 2)
  return (T.Semi pos t (T.Skip pos), u)

distrib :: Int -> Gen (T.Type, T.Type)
distrib n = do
  (t , u ) <- bisimPair (n `div` 4)
  (m1, m2) <- typeMapPair bisimPair (n `div` 4)
  p        <- arbitrary
  return
    ( T.Semi pos (T.Choice pos p m1) t
    , T.Choice pos p (Map.map (\v -> T.Semi pos v u) m2)
    )

assoc :: Int -> Gen (T.Type, T.Type)
assoc n = do
  (t, u) <- bisimPair (n `div` 6)
  (v, w) <- bisimPair (n `div` 6)
  (x, y) <- bisimPair (n `div` 6)
  return (T.Semi pos t (T.Semi pos v x), T.Semi pos (T.Semi pos u w) y)

commut :: Int -> Gen (T.Type, T.Type)  -- TODO: this axiom be distributed among all others
commut n = do
  (t, u) <- bisimPair (n `div` 4)
  return (u, t)

-- Lemma 3.5 _ Laws for mu-types (ICFP'16)

recRecL :: Int -> Gen (T.Type, T.Type)
recRecL n = do
  (t, u)                <- bisimPair (n `div` 2)
  xk@(K.KindBind _ x _) <- arbitrary
  yk@(K.KindBind _ y _) <- arbitrary
  let u' = Rename.renameType u -- this type will be in a substitution
  return
    ( T.Rec pos xk (T.Rec pos yk t)
    , T.Rec pos xk (Rename.subs (T.TypeVar pos x) y u')
    )

recRecR :: Int -> Gen (T.Type, T.Type)
recRecR n = do
  (t, u)                <- bisimPair (n `div` 2)
  xk@(K.KindBind _ x _) <- arbitrary
  yk@(K.KindBind _ y _) <- arbitrary
  let u' = Rename.renameType u -- this type will be in a substitution
  return
    ( T.Rec pos xk (T.Rec pos yk t)
    , T.Rec pos yk (Rename.subs (T.TypeVar pos y) x u')
    )

recFree :: Int -> Gen (T.Type, T.Type)
recFree n = do
  (t, u) <- bisimPair (n `div` 2)
  k      <- arbitrary
  return (T.Rec pos (K.KindBind pos freeTypeVar k) t, u)

-- alphaConvert :: Int -> Gen (Type, Type) -- (fixed wrt to ICFP'16)
-- alphaConvert n = do
--   (t, u) <- bisimPair n
--   (x, k) <- arbitrary
--   let y = freeTypeVar -- TODO: Here we need a genunine free var
--   return (Rec pos (KindBind pos x k) t,
--           Rec pos (KindBind pos y k) (Rename.subs (TypeVar pos y) x u))

subsOnBoth :: Int -> Gen (T.Type, T.Type)
subsOnBoth n = do
  (t, u) <- bisimPair (n `div` 4)
  (v, w) <- bisimPair (n `div` 4)
  let [t', u', v', w'] = Rename.renameTypes [t, u, v, w] -- these types will be in a substitution
  x <- arbitrary
  return (Rename.subs t' x v', Rename.subs u' x w')

unfoldt :: Int -> Gen (T.Type, T.Type)
unfoldt n = do
  (t, u) <- bisimPair (n `div` 2)
  let u' = Rename.renameType u -- this type will be unfolded
  xk <- arbitrary
  return (T.Rec pos xk t, Rename.unfold (T.Rec pos xk u'))

-- Arbitrary pairs of non-bisimilar types

data NonBisimPair = NonBisimPair T.Type T.Type

instance Show NonBisimPair where
  show (NonBisimPair t u) = show t ++ " not-bisimilar-to " ++ show u

instance Arbitrary NonBisimPair where
  arbitrary = do
    (t, u) <- sized nonBisimPair
    let [t', u'] = Rename.renameTypes [t, u]
    return $ NonBisimPair t' u'

nonBisimPair :: Int -> Gen (T.Type, T.Type)
nonBisimPair 0 =
  oneof
    -- Anti-axioms
        [skipMessage, varSkip, messageVar]
nonBisimPair n = oneof
    -- The various type constructors, excluding the "atoms": skip, message, var
  [semiPair nonBisimPair n, recPair nonBisimPair n, choicePair nonBisimPair n]

-- A few anti-axioms

skipMessage :: Gen (T.Type, T.Type)
skipMessage = do
  (p, b) <- arbitrary
  return (T.Skip pos, T.Message pos p b)

varSkip :: Gen (T.Type, T.Type)
varSkip = do
  x <- arbitrary
  return (T.TypeVar pos x, T.Skip pos)

messageVar :: Gen (T.Type, T.Type)
messageVar = do
  (p, b, x) <- arbitrary
  return (T.Message pos p b, T.TypeVar pos x)

messageInOut :: Gen (T.Type, T.Type)
messageInOut = do
  (p, b) <- arbitrary
  return (T.Message pos p b, T.Message pos (dual p) b)

dual T.In  = T.Out
dual T.Out = T.In
