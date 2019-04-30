import           Test.QuickCheck
import           Equivalence.Bisimulation
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

-- main = quickCheckWith stdArgs {maxSuccess = 1000} prop_bisimilar
-- main = verboseCheckWith stdArgs {maxSuccess = 1000} prop_bisimilar
main = quickCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar_trace
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_subs_kind_preservation1
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_norm_preserves_bisim
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_distribution
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_dual_convolution

-- Convenience

bisim :: Type -> Type -> Bool
bisim = bisimilar Map.empty
norm :: Type -> Type
norm = normalise Map.empty
pos = defaultPos

renameType :: Type -> Type
renameType t = head (renameList [t])

renameList :: [Type] -> [Type]
renameList ts = evalState (mapM (rename Map.empty) ts) (initialState "Renaming for QuickCheck")

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))
        -- TODO: This env should only contain the free vars of t; its
        -- kind may be SU
        
kinded :: Type -> Bool
kinded t = null (errors state)
  where state = execState (synthetise kindEnv t) (initialState "Quick Checking")
  
kindOf :: Type -> Maybe Kind
kindOf t
  | null (errors s) = Just k
  | otherwise       = Nothing
  where (k, s) = runState (synthetise kindEnv t) (initialState "Kind syntesis")

-- Bisimilar types are bisimilar

prop_bisimilar :: BisimPair -> Property
prop_bisimilar (BisimPair t u) = kinded t ==>
  evalState (return $ bisim t u) ()

prop_bisimilar_trace :: BisimPair -> Property
prop_bisimilar_trace (BisimPair t u) = kinded t ==>
  evalState (trace ("=> " ++ show t ++ " bisim " ++ show u) (return $ bisim t u)) ()

prop_self_bisimilar :: Type -> Property
prop_self_bisimilar t = kinded t ==>
  evalState (trace (show u ++ " self-bisimilar-to " ++ show v) (return $ bisim u v)) ()
  where [u, v] = renameList [t, t]

-- Normalisation preserves bisimilarity
prop_norm_preserves_bisim :: Type -> Property
prop_norm_preserves_bisim t = kinded t' ==> bisim u v
  -- evalState (trace (show u ++ " norm_preserves_bisim " ++ show v) (return $ bisim u v)) ()
  where t' = renameType t
        [u, v] = renameList [t, normalise Map.empty t']

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

-- Arbitrary Types

instance Arbitrary Multiplicity where
  arbitrary = elements [Un, Lin]

instance Arbitrary Polarity where
  arbitrary = elements [In, Out]

instance Arbitrary Pos where
  arbitrary = return pos

ids :: [String]            -- Type Variables
ids = ["x", "y", "z"]

freeTypeVar :: TypeVar
freeTypeVar = mkVar pos "∂"

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
  arbitrary = sized arbitrarySession -- Warning: not renamed

arbitrarySession :: Int -> Gen Type
arbitrarySession 0 = oneof
  [ liftM Skip arbitrary
  , liftM2 TypeVar arbitrary arbitrary
  , liftM3 Message arbitrary arbitrary arbitrary
  ]
arbitrarySession n = oneof
  [ liftM3 Semi arbitrary (arbitrarySession (n `div` 4)) (arbitrarySession (n `div` 4))
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

{-

This is a fully functional version that only applies one axiom to a
seed type

instance Arbitrary BisimPair where
  arbitrary = do
    t <- arbitrary
    (u, v) <- oneof $ map (\axiom -> axiom t t)
      [
    -- Lemma 3.4 _ Laws for sequential composition (ICFP'16)
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
      ]
    let [u', v'] = renameList [u, v]
    return $ BisimPair u' v'
-}

instance Arbitrary BisimPair where
  arbitrary = do
    t <- arbitrary -- The seed type
    n <- arbitrary -- The number of axioms to apply
    (u, v) <- arbitraryBisimPair (abs n) t t
    let [u', v'] = renameList [u, v]
    return $ BisimPair u' v'

arbitraryBisimPair :: Int -> Type -> Type -> Gen (Type, Type)
arbitraryBisimPair 0 t u = return (t, u)
arbitraryBisimPair n t u = do
  (t', u') <- arbitraryBisimPair (n - 1) t u
  oneof $ map (\axiom -> axiom t' u')
    [
    -- Lemma 3.4 _ Laws for sequential composition (ICFP'16)
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
    -- Commutativity
    , commut
    ]

-- The various axioms all share this signature
type GenBisimPair = Type -> Type -> Gen (Type, Type)

-- Lemma 3.4 _ Laws for sequential composition (ICFP'16)

skipt :: GenBisimPair
skipt t u = return (Semi pos (Skip pos) t, u)

tskip :: GenBisimPair
tskip t u = return (Semi pos t (Skip pos), t)

assoc :: GenBisimPair
assoc t u = do
  (v , w) <- arbitrary
  return (Semi pos t (Semi pos v w), Semi pos (Semi pos u v) w)

distrib :: GenBisimPair
distrib t u = do
  p <- arbitrary
  m <- sized arbitraryTypeMap
  return (Semi pos (Choice pos p m) t, Choice pos p (Map.map (\v -> Semi pos v u) m))

-- Lemma 3.5 _ Laws for mu-types (ICFP'16)

recrec :: GenBisimPair
recrec t u = do
  (xk@(TypeVarBind _ x _), yk@(TypeVarBind _ y _)) <- arbitrary
  return (Rec pos xk (Rec pos yk t), Rec pos xk (subs (TypeVar pos x) y (renameType u)))

recFree :: GenBisimPair
recFree t u = do
  k <- arbitrary
  return (Rec pos (TypeVarBind pos freeTypeVar k) t, u)
  -- Note: the rec-var must be distinct from the free variables of t

alphaConvert :: GenBisimPair -- (fixed wrt to ICFP'16)
alphaConvert t u = do
  (x, k) <- arbitrary
  let y = freeTypeVar
  return (Rec pos (TypeVarBind pos x k) t,
          Rec pos (TypeVarBind pos y k) (subs (TypeVar pos y) x u))

subsOnBoth :: GenBisimPair
subsOnBoth t u = do
  (v, x) <- arbitrary
  return (subs t x (renameType v), subs u x (renameType v))

unfoldt :: GenBisimPair
unfoldt t u = do
  xk <- arbitrary
  return (Rec pos xk t, unfold (Rec pos xk (renameType u)))

-- Commutativity

commut :: GenBisimPair
commut t u = return (u, t)
