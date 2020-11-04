{- |
Module      :  Equivalence.Normalisation
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pot
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

-}

module Equivalence.Normalisation
( Normalise(..)
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Base
import           Validation.Duality
import           Validation.Terminated (terminated)
import qualified Validation.Substitution as Substitution (unfold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Normalise t where
  normalise :: TypeEnv -> t -> t

-- Requires: t well-formed
-- normalise t = u implies
--   t is equivalent to u and
--   u is not a rec type and
--   u is not a name type and
--   if u is u1;u2, then u1 is not terminated
instance Normalise Type where
    -- Session types
  -- normalise _    t
  --   | terminated t = Skip (position t)
  normalise tenv (Semi _ t u)
    | terminated t = normalise tenv u
    | otherwise    = append (normalise tenv t) u
  normalise tenv t@Rec{} = normalise tenv (Substitution.unfold t)
    -- Type operators
--  normalise tenv (Dualof _ t) = (normalise tenv (dual t)
  normalise tenv (Dualof p t) = Dualof p (normalise tenv t) -- Lazy version 
  normalise tenv (TypeName _ a) = normalise tenv t
    where (_, TypeScheme _ [] t) = tenv Map.! a -- TODO: type/data may be polymorphic
    -- Otherwise: Basic, Fun, PairType, Datatype, Skip, Message, Choice, TypeVar
  normalise _ t = t

append :: Type -> Type -> Type
append (Skip _)     t = t
append t     (Skip _) = t
append (Semi p t u) v = Semi p t (append u v)
append t            u = Semi (position t) t u

instance Normalise TypeScheme where
  normalise tenv (TypeScheme p bs t) = TypeScheme p bs (normalise tenv t)
