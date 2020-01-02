-- NOT WORKING!!!

module TestInvalidTypes
( prop_not_bisimilar
-- , kinded
) where

import           Test.QuickCheck
import           Equivalence.Equivalence
import           Equivalence.Bisimulation
import           Equivalence.Normalisation
import           Validation.Substitution
import           Validation.Kinding
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import           Utils.FreestState
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Map.Strict as Map
import           ArbitraryTypes

main = quickCheckWith stdArgs {maxSuccess = 1000} prop_not_bisimilar -- prop_equivalent
-- main = quickCheckWith stdArgs {maxSuccess = 10000} prop_distribution
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

pos :: Pos
pos = defaultPos

kindEnv :: KindEnv
kindEnv = Map.fromList (zip (map (mkVar pos) ids) (repeat (kindSL pos)))
        -- TODO: This env should only contain the free vars of t; plus
        -- its kind may be SU

kinded :: Type -> Bool
kinded t = null (errors s)
  where (_, s) = runState (synthetise kindEnv t) (initialState "Kind synthesis")

-- Bisimilar types are bisimilar
prop_not_bisimilar :: NonBisimPair -> Property
prop_not_bisimilar (NonBisimPair t u) = kinded t && kinded u ==> not (t `bisim` u)

-- Distribution

prop_distribution :: NonBisimPair -> Property
prop_distribution (NonBisimPair t _) = kinded t ==>
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
