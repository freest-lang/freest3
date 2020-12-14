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
  )
where

import           Syntax.Type                   as T
import           Syntax.Base
import           Validation.Terminated         ( terminated )
import qualified Validation.Substitution       as Substitution
                                               ( unfold )
import           Utils.Error                   ( internalError )
import qualified Data.Map.Strict               as Map

class Normalise t where
  normalise :: T.TypeEnv -> t -> t

instance Normalise T.Type where
    -- Session types
  normalise tenv (T.Semi _ t u) | terminated t = normalise tenv u
                              | otherwise    = append (normalise tenv t) u
  normalise tenv t@T.Rec{} = normalise tenv (Substitution.unfold t)
    -- Type operators
--  normalise tenv (Dualof _ t) = (normalise tenv (dual t)
  normalise _ (T.Dualof _ t) =
    internalError "Equivalence.Normalisation.normalise" t
--  normalise tenv (TypeName _ a) = normalise tenv (snd $ tenv Map.! a) -- TODO: type/data may be polymorphic
  -- normalise tenv v@(T.Var _ a) =
  --   case tenv Map.!? a of
  --     Just t  -> normalise tenv (snd t) -- TODO: type/data may be polymorphic
  --     Nothing -> v
    -- Otherwise: Basic, Fun, PairType, Datatype, Skip, Message, Choice, TypeVar
  normalise _ t = t

append :: T.Type -> T.Type -> T.Type
append (T.Skip _)     t          = t
append t              (T.Skip _) = t
append (T.Semi p t u) v          = T.Semi p t (append u v)
append t              u          = T.Semi (pos t) t u

-- instance Normalise TypeScheme where
--   normalise tenv (TypeScheme p bs t) = TypeScheme p bs (normalise tenv t)
