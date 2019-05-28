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
, terminated
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Base
import           Validation.Substitution
import qualified Data.Map.Strict as Map

class Normalise t where
  normalise :: TypeEnv -> t -> t

-- Requires: t well-kinded
-- normalise t = u implies
--   t is equivalent to u and
--   u is not a rec type and
--   u is not a name type and
--   if u is u1;u2, then u1 is not terminated
instance Normalise Type where
    -- Session types
  normalise tenv (Semi p t u)
    | terminated t = normalise tenv u
    | otherwise    = append (normalise tenv t) u
    -- case normalise tenv t of
    --   (Skip _)  -> normalise tenv u
    --   otherwise -> Semi p (normalise tenv t) u
    -- Functional or session
  normalise tenv t@(Rec _ _ _) = normalise tenv (unfold t) -- diverges, obviously
    -- Type operators
  normalise tenv (Dualof _ t) = normalise tenv (dual t)
  normalise tenv (TypeName _ a) = normalise tenv t
    where (_, TypeScheme _ [] t) = tenv Map.! a -- TODO: type/data may be polymorphic
    -- Otherwise: Basic, Fun, PairType, Datatype, Skip, Message, Choice, TypeVar
  normalise tenv t = t

append :: Type -> Type -> Type
append (Skip _)       t = t
append t       (Skip _) = t
append (Semi p t u)   v = Semi p t (append u v)
append t              u = Semi (position t) t u

-- As in the ICFP'16 paper, except that no substitution is applied to Rec types
terminated :: Type ->  Bool
terminated (Skip _)     = True
terminated (Semi _ t u) = terminated t && terminated u
terminated (Rec _ _ t)  = terminated t
terminated _            = False

instance Normalise TypeScheme where
  normalise tenv (TypeScheme p bs t) = TypeScheme p bs (normalise tenv t)


{- A "better" terminated predicate should be such that only free variables *of kind SU* are terminated

terminated :: KindEnv -> Type ->  Bool
terminated kEnv t = term t
  where
  term (Skip _)      = True
  term (Semi _ s t)  = term s && term t
  term (Rec _ _ t)   = term t
  term (TypeVar _ x) = isUn (kEnv Map.! x)
  term _             = False
-}

{- This is part of a more ambitious normalisation procedure

-}
