{- |
Module      :  Equivalence.Normalisation
Description :  The normalisation of a type (and type scheme)
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt

The normalisations instances for Types and type schemes.

-}

module Equivalence.Normalisation
( Normalise(..)
, terminated
) where

import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.TypeVariables
import           Syntax.Base
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Normalise t where
  normalise :: TypeEnv -> t -> t

instance Normalise TypeScheme where
  normalise tenv (TypeScheme p bs t) = TypeScheme p bs (normalise tenv t)

-- Requires: t is contrative (well-kinded as well?)
-- normalise t = u implies
--   t is equivalent to u and
--   u is not a rec type and
--   u is not a name type and
--   if u is u1;u2, then u1 is not checked
instance Normalise Type where
    -- Session types
  normalise tenv (Semi _ t u)
    | terminated t = normalise tenv u
    | otherwise   = append (normalise tenv t) u
    -- Functional or session
  normalise tenv t@(Rec _ _ _) = normalise tenv (unfold t)
    -- Type operators
  normalise tenv (Dualof _ t) = normalise tenv (dual t)
  normalise tenv (TypeName _ a) = normalise tenv t
    where (_, TypeScheme _ [] t) = tenv Map.! a
    -- Otherwise: Basic, Fun, PairType, Datatype, Skip, Message, Choice, TypeVar
  normalise tenv t = t

append :: Type -> Type -> Type
append (Skip _)       t = t
append t       (Skip _) = t
append (Semi p t u)   v = Semi p t (append u v)
append t              u = Semi (position t) t u

terminated :: Type ->  Bool
terminated = isChecked Set.empty
  where
  isChecked _ (Skip _)                      = True
  isChecked v (Semi _ s t)                  = isChecked v s && isChecked v t
  isChecked v (Rec _ (TypeVarBind _ x _) t) = isChecked (Set.insert x v) t
  -- Only free variables are terminated.
  isChecked v (TypeVar _ x)                 = Set.notMember x v
  isChecked _ _                             = False
