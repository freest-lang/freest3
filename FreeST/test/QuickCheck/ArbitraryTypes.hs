{-# LANGUAGE FlexibleInstances #-}

module ArbitraryTypes
  ( BisimPair(..)
  , NonBisimPair(..)
  , ids
  )
where

import           Test.QuickCheck
import qualified Syntax.Type                   as T
import qualified Syntax.Kind                   as K
import           Syntax.Base             hiding ( pos )
-- import           Parse.Unparser
import qualified Validation.Rename             as Rename
import qualified Data.Map.Strict               as Map
import           Control.Monad

pos :: Pos
pos = defaultPos

-- | Type variables
ids :: [String]
ids = ["x", "y", "z"]

freeTypeVar :: Variable
freeTypeVar = mkVar pos "δ"

-- | Labels in choices
choices :: [String]
choices = ["A", "B", "C"]

instance Arbitrary Multiplicity where
  arbitrary = elements [Un, Lin]

instance Arbitrary T.Polarity where
  arbitrary = elements [T.In, T.Out]

instance Arbitrary T.View where
  arbitrary = elements [T.External, T.Internal]
  
instance Arbitrary T.Sort where
  arbitrary = elements [
    -- T.Record, T.Variant, -- These two yield too many 'precondition false'
    T.Choice T.External, T.Choice T.Internal]

instance Arbitrary Variable where
  arbitrary = do
    id <- elements ids
    return $ mkVar pos id

instance Arbitrary K.Kind where
  arbitrary = elements $ K.su pos : replicate 9 (K.sl pos) -- 90% of SL

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bind a b) where
  arbitrary = liftM4 Bind (return pos) arbitrary arbitrary arbitrary

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
bisimPair 0 = oneof [skipPair, varPair]
bisimPair n = oneof
  [
  -- Some functional type constructors
    intPair
  , charPair
    -- Bool, String, Unit may not be needed
  , arrowPair bisimPair n
  , pairPair bisimPair n
  -- The various session types constructors
  , skipPair
  , semiPair bisimPair n
  , messagePair bisimPair n
  , choicePair bisimPair n
  -- The recursive type constructors
  , recPair bisimPair n
  , varPair
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

intPair :: Gen (T.Type, T.Type)
intPair = return (T.Int pos, T.Int pos)

charPair :: Gen (T.Type, T.Type)
charPair = return (T.Char pos, T.Char pos)

messagePair :: PairGen -> Int -> Gen (T.Type, T.Type)
messagePair pairGen n = do
  pol <- arbitrary
  (t, u) <- pairGen (n `div` 4) -- HO CFST
  return (T.Message pos pol t, T.Message pos pol u)
  -- First Order
  -- t <- elements [T.Int pos, T.Char pos, T.Bool pos, T.Unit pos]
  -- return (T.Message pos pol t, T.Message pos pol t)

varPair :: Gen (T.Type, T.Type)
varPair = do
  a <- arbitrary
  return (T.Var pos a, T.Var pos a)

semiPair :: PairGen -> Int -> Gen (T.Type, T.Type)
semiPair pairGen n = do
  (t, u) <- pairGen (n `div` 8)
  (v, w) <- pairGen (n `div` 8)
  return (T.Semi pos t v, T.Semi pos u w)

choicePair :: PairGen -> Int -> Gen (T.Type, T.Type)
choicePair pairGen n = do
  c        <- arbitrary
  (m1, m2) <- typeMapPair pairGen n
  return (T.Almanac pos c m1, T.Almanac pos c m2)

typeMapPair :: PairGen -> Int -> Gen (T.TypeMap, T.TypeMap)
typeMapPair pairGen n = do
  k     <- choose (1, length choices)
  pairs <- vectorOf k $ fieldPair (n `div` k)
  let (f1, f2) = unzip pairs
  return (Map.fromList f1, Map.fromList f2)
 where
  fieldPair :: Int -> Gen ((Variable, T.Type), (Variable, T.Type))
  fieldPair n = do
    (t, u) <- pairGen (n `div` 4)
    l      <- elements choices -- arbitrary
    let x = mkVar pos l
    return ((x, t), (x, u))

-- The various type constructors (except forall

arrowPair :: PairGen -> Int -> Gen (T.Type, T.Type)
arrowPair pairGen n = do
  mult <- arbitrary
  (t, u) <- pairGen (n `div` 8)
  (v, w) <- pairGen (n `div` 8)
  return (T.Arrow pos mult t v, T.Arrow pos mult u w)

pairPair :: PairGen -> Int -> Gen (T.Type, T.Type)
pairPair pairGen n = do
  (t, u) <- pairGen (n `div` 8)
  (v, w) <- pairGen (n `div` 8)
  return (T.Pair pos t v, T.Pair pos u w)

-- Recursion

recPair :: PairGen -> Int -> Gen (T.Type, T.Type)
recPair pairGen n = do
  (t, u) <- pairGen (n `div` 4)
  a      <- arbitrary
  k      <- arbitrary
  return (T.Rec pos (Bind pos a k t), T.Rec pos (Bind pos a k u))

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
    ( T.Semi pos (T.Almanac pos (T.Choice p) m1) t
    , T.Almanac pos (T.Choice p) (Map.map (\v -> T.Semi pos v u) m2)
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
  a <- arbitrary
  b <- arbitrary
  k <- arbitrary
  let u' = Rename.renameType u -- this type will be in a substitution
  return
    ( T.Rec pos (Bind pos a k (T.Rec pos (Bind pos b k t)))
    , T.Rec pos (Bind pos a k (Rename.subs (T.Var pos a) b u'))
    )

recRecR :: Int -> Gen (T.Type, T.Type)
recRecR n = do
  (t, u)                <- bisimPair (n `div` 2)
  a <- arbitrary
  b <- arbitrary
  k <- arbitrary
  let u' = Rename.renameType u -- this type will be in a substitution
  return
    ( T.Rec pos (Bind pos a k (Rename.subs (T.Var pos a) b u'))
    , T.Rec pos (Bind pos a k (T.Rec pos (Bind pos b k t)))
    )

recFree :: Int -> Gen (T.Type, T.Type)
recFree n = do
  (t, u) <- bisimPair (n `div` 2)
  k      <- arbitrary
  return (T.Rec pos (Bind pos freeTypeVar k t), u)

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
  a <- arbitrary
  k <- arbitrary
  return (T.Rec pos (Bind pos a k t), Rename.unfold (T.Rec pos (Bind pos a k u')))

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
  return (T.Var pos x, T.Skip pos)

messageVar :: Gen (T.Type, T.Type)
messageVar = do
  (p, b, x) <- arbitrary
  return (T.Message pos p b, T.Var pos x)

messageInOut :: Gen (T.Type, T.Type)
messageInOut = do
  (p, b) <- arbitrary
  return (T.Message pos p b, T.Message pos (dual p) b)

dual T.In  = T.Out
dual T.Out = T.In
