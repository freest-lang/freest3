{- |
Module      :  Equivalence.Normalisation
Description :  Unfold recursive types and terminated types
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pot
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Unfold recursive types and get rid of terminated types, in such a way that the
top level constructor of the resulting type is not a rec and not Skip;T.
-}

module Equivalence.Normalisation
  ( normalise
  )
where

import           Syntax.Base                    ( pos )
import qualified Syntax.Type                   as T
import           Validation.Terminated          ( terminated )
import           Validation.Substitution        ( unfold )
import           Util.Error                    ( internalError )

normalise :: T.Type -> T.Type
-- Session types
normalise (T.Semi _ t u)
  | terminated t = normalise u
  | otherwise    = append (normalise t) u
normalise t@T.Rec{} = normalise (unfold t)
 -- Type operators
normalise t@T.Dualof{} = internalError "Equivalence.Normalisation.normalise" t
-- Otherwise: Basic, Fun, PairType, Datatype, Skip, Message, Choice, TypeVar
normalise t = t

append :: T.Type -> T.Type -> T.Type
append (T.Skip _)     t          = t
append t              (T.Skip _) = t
append (T.Semi p t u) v          = T.Semi p t (append u v)
append t              u          = T.Semi (pos t) t u
