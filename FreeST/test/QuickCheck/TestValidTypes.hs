module TestValidTypes
  ( prop_bisimilar
  , prop_distribution
  , kinded
  , nodes
  )
where

import           Syntax.Base
import qualified Syntax.Type                  as T
import           Syntax.Kind                  as K
import           Validation.Kinding
-- import           Equivalence.Normalisation
import           Equivalence.Equivalence
import           Bisimulation.Bisimulation
import           Util.FreestState
import           Control.Monad.State
import           ArbitraryTypes
import qualified Data.Map.Strict               as Map
import           Test.QuickCheck
-- import           Test.QuickCheck.Random       ( mkQCGen )

-- main = verboseCheckWith
--   stdArgs { maxSuccess = 271, replay = Just (mkQCGen 1095646480, 0) }
--   prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 20000} prop_bisimilar -- prop_equivalent
-- main = verboseCheckWith stdArgs {maxSuccess = 10000} prop_distribution
-- main = quickCheckWith stdArgs {maxSuccess = 10000, replay = Just (mkQCGen 42, 0)} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_subs_kind_preservation1
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_norm_preserves_bisim
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_distribution
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_dual_convolution

-- Convenience

equiv :: T.Type -> T.Type -> Bool
equiv = equivalent kindEnv

-- norm :: T.Type -> T.Type
-- norm = normalise Map.empty

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar defaultPos) ids) (repeat (K.sl defaultPos)))
        -- TODO: This env should only contain the free vars of t; plus
        -- its kind may be SU

kinded :: T.Type -> Bool
kinded t =
  null $ errors $ execState (synthetise kindEnv t) (initialState) --  "Kind synthesis")


-- Bisimilar types are bisimilar
prop_bisimilar :: BisimPair -> Property
prop_bisimilar (BisimPair t u) = kinded t && kinded u ==> t `bisimilar` u

-- Equivalence
prop_equivalent :: BisimPair -> Property
prop_equivalent (BisimPair t u) = kinded t && kinded u ==> t `equiv` u

-- Normalisation preserves bisimilarity
-- prop_norm_preserves_bisim :: Type -> Property
-- prop_norm_preserves_bisim t = kinded t ==> bisim u v
--   where t' = renameType t
--         [u, v] = renameTypes [t, normalise Map.empty t']

-- Duality is a convolution
-- prop_dual_convolution :: Type -> Property
-- prop_dual_convolution t = kinded t ==> dual (dual t) == t

-- Lemma 3.1(ii) _ Substitution and kind preservation (ICFP'16)
-- prop_subs_kind_preservation2 :: TypeVar ->Kind -> Type -> Property
-- prop_subs_kind_preservation2 x k t =
--   isJust k1 ==> k1 == kindOf (unfold u)
--   where u = renameType $ Rec defaultPos (TypeVarBind defaultPos x k) t
--         k1 = kindOf u

-- Lemma 3.3 _ Laws for terminated communication (ICFP'16)
-- prop_terminated1 :: Type -> Type -> Property
-- prop_terminated1 t u =
--   kinded t ==> not (terminated t && terminated u) || bisim t u

-- Laws for terminated communication (bonus)
-- prop_terminated2 :: Type -> Property
-- prop_terminated2 t =
--   kinded t' ==> terminated t' == bisim t' (Skip defaultPos)
--   where t' = renameType t

-- Distribution

prop_distribution :: BisimPair -> Property
prop_distribution (BisimPair t u) =
  kinded t && kinded u ==> collect (nodes t + nodes u) $ tabulate
    "Type constructors"
    [constr t]
    True

-- The number of nodes in a type
nodes :: T.Type -> Int
nodes (T.Semi   _ t u) = 1 + nodes t + nodes u
nodes (T.Choice _ _ m) = 1 + Map.foldr (\t acc -> nodes t + acc) 0 m
nodes (T.Rec    _ (K.Bind _ _ _ t)) = 1 + nodes t
-- Skip, Message, TypeVar
nodes _                = 1

-- The constructor of a type
constr :: T.Type -> String
constr T.Int{}  = "Int"
constr T.Char{} = "Char"
constr T.Unit{} = "Unit"
constr T.Bool{} = "Bool"
constr T.Arrow{} = "Fun"
constr T.Pair{} = "Pair"
constr T.Variant{} = "Datatype"
constr T.Skip{} = "Skip"
constr T.Semi{} = "Semi"
constr T.Message{} = "Message"
constr T.Choice{} = "Choice"
constr T.Rec{} = "Rec"
constr T.Var{} = "TypeVar"
constr T.Dualof{} = "Dualof"
