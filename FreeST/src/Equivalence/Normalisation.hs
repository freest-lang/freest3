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

import           Syntax.Base                    ( Pos )
import qualified Syntax.Type                   as T
import           Validation.Terminated          ( terminated )
import           Validation.Substitution        ( unfold )
import           Util.Error                    ( internalError )
import Validation.Substitution (subs)
import qualified Syntax.Kind as K

normalise :: T.Type -> T.Type
-- Session types
normalise (T.Semi p t u)
  | terminated t = normalise u
  | otherwise    = append p (normalise t) u
normalise u@(T.Rec _ (K.Bind _ x _ t)) = subs u x (normalise t)
-- Type operators
normalise t@T.Dualof{} = internalError "Equivalence.Normalisation.normalise" t
-- Otherwise: Basic, Fun, PairType, Datatype, Skip, Message, Choice, TypeVar
normalise t = t

append :: Pos -> T.Type -> T.Type -> T.Type
--append _ (T.Skip _)     t          = t
append _ t              (T.Skip _) = t
append p (T.Semi p1 t u) v          = T.Semi p1 t (append p u v)
append p t              u          = T.Semi p t u
-- append t              u          = T.Semi (pos t) t u
