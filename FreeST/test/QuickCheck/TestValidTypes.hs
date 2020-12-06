module TestValidTypes
( prop_bisimilar
, prop_distribution
, kinded
, nodes
) where

import           Test.QuickCheck
import           Test.QuickCheck.Random (mkQCGen)
import           Equivalence.Equivalence
import           Equivalence.Normalisation
import           Bisimulation.Bisimulation
import           Validation.Substitution
import           Validation.Kinding
import qualified Syntax.Type as T
import           Syntax.Kind
import           Syntax.TypeVariables
import           Syntax.Base hiding (pos)
-- import           Validation.Duality
import           Utils.FreestState
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Map.Strict as Map
import           ArbitraryTypes

main = verboseCheckWith stdArgs {maxSuccess = 271, replay = Just (mkQCGen 1095646480, 0)} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 20000} prop_bisimilar -- prop_equivalent
-- main = verboseCheckWith stdArgs {maxSuccess = 10000} prop_distribution
-- main = quickCheckWith stdArgs {maxSuccess = 10000, replay = Just (mkQCGen 42, 0)} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_bisimilar
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_subs_kind_preservation1
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_norm_preserves_bisim
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_distribution
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_dual_convolution

-- Convenience

bisim :: T.Type -> T.Type -> Bool
bisim = Equivalence.Equivalence.bisimilar Map.empty

equiv :: T.Type -> T.Type -> Bool
equiv = equivalent Map.empty kindEnv

norm :: T.Type -> T.Type
norm = normalise Map.empty

pos :: Pos
pos = defaultPos

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))
        -- TODO: This env should only contain the free vars of t; plus
        -- its kind may be SU

kinded :: T.Type -> Bool
kinded t = null (errors s)
  where (_, s) = runState (synthetise kindEnv t) (initialState "Kind synthesis")

-- Bisimilar types are bisimilar
prop_bisimilar :: BisimPair -> Property
prop_bisimilar (BisimPair t u) = kinded t && kinded u ==> t `bisim` u

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
--   where u = renameType $ Rec pos (TypeVarBind pos x k) t
--         k1 = kindOf u

-- Lemma 3.3 _ Laws for terminated communication (ICFP'16)
-- prop_terminated1 :: Type -> Type -> Property
-- prop_terminated1 t u =
--   kinded t ==> not (terminated t && terminated u) || bisim t u

-- Laws for terminated communication (bonus)
-- prop_terminated2 :: Type -> Property
-- prop_terminated2 t =
--   kinded t' ==> terminated t' == bisim t' (Skip pos)
--   where t' = renameType t

-- Distribution

prop_distribution :: BisimPair -> Property
prop_distribution (BisimPair t u) = kinded t && kinded u ==>
  collect (nodes t + nodes u) $
  tabulate "Type constructors" [constr t] True

-- The number of nodes in a type
nodes :: T.Type -> Int
nodes (T.Semi _ t u)   = 1 + nodes t + nodes u
nodes (T.Choice _ _ m) = 1 + Map.foldr (\t acc -> nodes t + acc) 0 m
nodes (T.Rec _ _ t)    = 1 + nodes t
-- Skip, Message, TypeVar
nodes _              = 1

-- The constructor of a type
constr :: T.Type -> String
constr T.IntType{} = "Int"
constr T.CharType{} = "Char"
constr T.UnitType{} = "Unit"
constr T.BoolType{} = "Bool"
constr T.Fun {} = "Fun"
constr T.PairType{} = "PairType"
constr T.Datatype{} = "Datatype"
constr T.Skip{} = "Skip"
constr T.Semi{} = "Semi"
constr T.Message{} = "Message"
constr T.Choice{} = "Choice"
constr T.Rec{} = "Rec"
constr T.TypeVar{} = "TypeVar"
constr T.TypeName{} = "TypeName"
constr T.Dualof{} = "Dualof"
