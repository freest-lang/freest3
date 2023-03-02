{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import           Validation.Contractive
import qualified Validation.Rename             as Rename
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Validation.Terminated (terminated)

pos :: Span
pos = defaultSpan

-- | Type variables
ids :: [String]
ids = ["a", "b", "c", "d", "e", "f", "g", "h", "i"]

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
  arbitrary = elements $ K.us pos : replicate 9 (K.ls pos) -- 90% of SL

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bind a b) where
  arbitrary = liftM4 Bind (return pos) arbitrary arbitrary arbitrary

-- Arbitrary pairs of bisimilar types

data BisimPair = BisimPair T.Type T.Type

instance Show BisimPair where
  show (BisimPair t u) = show t ++ " bisimilar-to " ++ show u

instance Arbitrary BisimPair where
  arbitrary = do
    (t, u) <- sized (bisimPair K.Top Set.empty)
    let [t', u'] = Rename.renameTypes [t, u]
    return $ BisimPair t' u'

instance Arbitrary T.Type where
  arbitrary = do
    (BisimPair _ t) <- arbitrary
    return t

type PairGen = Set.Set Variable -> Int -> Gen (T.Type, T.Type)

bisimPair :: K.PreKind -> PairGen 
-- Top types
bisimPair K.Top cVars 0 = 
  oneof [ bisimPair K.Session cVars 0 -- Top types include session types
        , oneof [ intPair 
                , charPair 
                , varPair (bisimPair K.Top) cVars
                ]
        ]
bisimPair K.Top cVars n = 
  oneof [ bisimPair K.Session cVars n -- Top types include session types
        , oneof [ intPair
                , charPair
                , varPair    (bisimPair K.Top) cVars 
                , arrowPair  cVars n
                , pairPair   cVars n
                , recRecL    (bisimPair K.Top) cVars n
                , recRecR    (bisimPair K.Top) cVars n
                , recFree    (bisimPair K.Top) cVars n
                , subsOnBoth (bisimPair K.Top) cVars n
                , unfoldt    (bisimPair K.Top) cVars n
                ]
        ]
-- Session types
bisimPair K.Session cVars 0 =
  oneof [ skipPair
        , endPair
        , varPair (bisimPair K.Session) cVars
        ]
bisimPair K.Session cVars n = 
  oneof [ skipPair
        , endPair
        , varPair     (bisimPair K.Session) cVars
        , semiPair    (bisimPair K.Session) cVars n
        , messagePair cVars n
        , choicePair  (bisimPair K.Session) cVars n
        -- The recursive type constructors
        , recPair     (bisimPair K.Session) cVars n
        , varPair     (bisimPair K.Session) cVars -- repeated?
          -- Lemma 3.4 _ Laws for sequential composition (ICFP'16)
        , skipT       cVars n
        , tSkip       cVars n
        , endT cVars n
        , distrib cVars n
        , assoc       cVars n
        , commut      (bisimPair K.Session) cVars n
          -- Lemma 3.5 _ Laws for mu-types (ICFP'16)
        , recRecL     (bisimPair K.Session) cVars n
        , recRecR     (bisimPair K.Session) cVars n
        , recFree     (bisimPair K.Session) cVars n
          -- , alphaConvert n
        , subsOnBoth  (bisimPair K.Session) cVars n
        , unfoldt     (bisimPair K.Session) cVars n
        ]

-- The various session type constructors

skipPair :: Gen (T.Type, T.Type)
skipPair = return (T.Skip pos, T.Skip pos)

endPair :: Gen (T.Type, T.Type)
endPair = return (T.End pos, T.End pos)

intPair :: Gen (T.Type, T.Type)
intPair = return (T.Int pos, T.Int pos)

charPair :: Gen (T.Type, T.Type)
charPair = return (T.Char pos, T.Char pos)

messagePair :: PairGen
messagePair n cVars = do
  pol <- arbitrary
  (t, u) <- bisimPair K.Top n cVars --  HO CFST; n instead of (n `div` 4), otherwise lots of Skip and End
  return (T.Message pos pol t, T.Message pos pol u)

varPair :: PairGen -> Set.Set Variable -> Gen (T.Type, T.Type)
varPair altPairGen cVars = do
  a <- arbitrary
  if a `Set.member` cVars
    then altPairGen cVars 1
    else return (T.Var pos a, T.Var pos a)

semiPair :: PairGen -> PairGen 
semiPair pairGen cVars n = do 
  (t1, t2) <- pairGen cVars (n `div` 2)
  (u1, u2) <- pairGen (if terminated t1 then cVars else Set.empty) -- C-Seq1 / C-Seq2
                      (n `div` 2)
  return (T.Semi pos t1 u1, T.Semi pos t2 u2)


choicePair :: PairGen -> PairGen
choicePair pairGen cVars n = do
  c        <- arbitrary
  (m1, m2) <- typeMapPair pairGen cVars n
  return (T.Almanac pos c m1, T.Almanac pos c m2)

typeMapPair :: PairGen -> Set.Set Variable -> Int -> Gen (T.TypeMap, T.TypeMap)
typeMapPair pairGen cVars n = do
  k     <- choose (1, length choices)
  pairs <- vectorOf k $ fieldPair (n `div` k)
  let (f1, f2) = unzip pairs
  return (Map.fromList f1, Map.fromList f2)
 where
  fieldPair :: Int -> Gen ((Variable, T.Type), (Variable, T.Type))
  fieldPair n = do
    (t, u) <- pairGen cVars (n `div` 4)
    l      <- elements choices -- arbitrary
    let x = mkVar pos l
    return ((x, t), (x, u))

-- The various type constructors (except forall)

arrowPair :: PairGen
arrowPair cVars n = do
  mult <- arbitrary
  (t, u) <- bisimPair K.Top cVars (n `div` 8)
  (v, w) <- bisimPair K.Top cVars (n `div` 8)
  return (T.Arrow pos mult t v, T.Arrow pos mult u w)

pairPair :: PairGen
pairPair cVars n = do
  (t, u) <- bisimPair K.Top cVars (n `div` 8)
  (v, w) <- bisimPair K.Top cVars (n `div` 8)
  return (T.tuple pos [t,v], T.tuple pos [u,w])

-- Recursion

recPair :: PairGen -> PairGen
recPair pairGen cVars n = do
  a      <- arbitrary
  k      <- arbitrary
  (t, u) <- pairGen (Set.insert a cVars) n
  return (T.Rec pos (Bind pos a k t), T.Rec pos (Bind pos a k u))
  -- return $ if contractive Set.empty a t && contractive Set.empty a u
  --          then (T.Rec pos (Bind pos a k t), T.Rec pos (Bind pos a k u))
  --          else (t, u)

-- Lemma 3.4 _ Laws for sequential composition (ICFP'16)

skipT :: PairGen
skipT cVars n = do
  (t, u) <- bisimPair K.Session cVars n --(n `div` 2)
  return (T.Semi pos (T.Skip pos) t, u)

tSkip :: PairGen
tSkip cVars n = do
  (t, u) <- bisimPair K.Session cVars n -- (n `div` 2)
  return (T.Semi pos t (T.Skip pos), u)

endT :: PairGen
endT cVars n = do
  (t, u) <- bisimPair K.Session cVars (n `div` 2)
  return (T.Semi pos (T.End pos) t, T.End pos)

distrib :: PairGen
distrib cVars n = do
  (t , u ) <- bisimPair K.Session cVars (n `div` 2)
  (m1, m2) <- typeMapPair (bisimPair K.Session) cVars (n `div` 2)
  p        <- arbitrary
  return
    ( T.Semi pos (T.Almanac pos (T.Choice p) m1) t
    , T.Almanac pos (T.Choice p) (Map.map (\v -> T.Semi pos v u) m2)
    )

assoc :: PairGen
assoc cVars n = do
  (t1, t2) <- bisimPair K.Session cVars (n `div` 6)
  let cVars' = if terminated t1 then cVars else Set.empty
  (u1, u2) <- bisimPair K.Session cVars (n `div` 6)
  (v1, v2) <- bisimPair K.Session cVars (n `div` 6)
  return (T.Semi pos t1 (T.Semi pos u1 v1), T.Semi pos (T.Semi pos t2 u2) v2)

commut :: PairGen -> PairGen  -- TODO: this axiom be distributed among all others
commut pairGen cVars n = do
  (t, u) <- pairGen cVars (n `div` 4)
  return (u, t)

-- Lemma 3.5 _ Laws for mu-types (ICFP'16)

recRecL :: PairGen -> PairGen
recRecL pairGen cVars n = do
  a <- arbitrary
  b <- arbitrary
  k <- arbitrary
  (t, u) <- pairGen (Set.union (Set.fromList[a, b]) cVars) (n `div` 2)
  let u' = Rename.renameType u -- this type will be in a substitution
  return
    ( T.Rec pos (Bind pos a k (T.Rec pos (Bind pos b k t)))
    , T.Rec pos (Bind pos a k (Rename.subs (T.Var pos a) b u'))
    )

recRecR :: PairGen -> PairGen
recRecR pairGen cVars n = do
  a <- arbitrary
  b <- arbitrary
  k <- arbitrary
  (t, u) <- pairGen (Set.union (Set.fromList[a, b]) cVars) (n `div` 2)
  let u' = Rename.renameType u -- this type will be in a substitution
  return
    ( T.Rec pos (Bind pos a k (Rename.subs (T.Var pos a) b u'))
    , T.Rec pos (Bind pos a k (T.Rec pos (Bind pos b k t)))
    )

recFree :: PairGen -> PairGen
recFree pairGen cVars n = do
  (t, u) <- pairGen cVars (n `div` 2)
  k      <- arbitrary
  return (T.Rec pos (Bind pos freeTypeVar k t), u)

-- alphaConvert :: Int -> Gen (Type, Type) -- (fixed wrt to ICFP'16)
-- alphaConvert n = do
--   (t, u) <- bisimPair n
--   (x, k) <- arbitrary
--   let y = freeTypeVar -- TODO: Here we need a genunine free var
--   return (Rec pos (KindBind pos x k) t,
--           Rec pos (KindBind pos y k) (Rename.subs (TypeVar pos y) x u))

subsOnBoth :: PairGen -> PairGen
subsOnBoth pairGen cVars n = do
  (t, u) <- pairGen cVars (n `div` 4)
  (v, w) <- pairGen cVars (n `div` 4)
  let [t', u', v', w'] = Rename.renameTypes [t, u, v, w] -- these types will be in a substitution
  x <- arbitrary
  return (Rename.subs t' x v', Rename.subs u' x w')

unfoldt :: PairGen -> PairGen
unfoldt pairGen cVars n = do
  a <- arbitrary
  k <- arbitrary
  (t, u) <- pairGen (Set.insert a  cVars) (n `div` 2)
  let u' = Rename.renameType u -- this type will be unfolded
  return (T.Rec pos (Bind pos a k t), Rename.unfold (T.Rec pos (Bind pos a k u')))

-- Arbitrary pairs of non-bisimilar types

data NonBisimPair = NonBisimPair T.Type T.Type

instance Show NonBisimPair where
  show (NonBisimPair t u) = show t ++ " not-bisimilar-to " ++ show u

instance Arbitrary NonBisimPair where
  arbitrary = do
    (t, u) <- sized (nonBisimPair Set.empty)
    let [t', u'] = Rename.renameTypes [t, u]
    return $ NonBisimPair t' u'

nonBisimPair :: PairGen
nonBisimPair cVars 0 =
  oneof
    -- Anti-axioms
        [skipMessage, varSkip, messageVar]
nonBisimPair cVars n = oneof
    -- The various type constructors, excluding the "atoms": skip, message, var
  [semiPair nonBisimPair cVars n, recPair nonBisimPair cVars n, choicePair nonBisimPair cVars n]

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