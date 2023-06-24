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

import           Syntax.Base                    ( Span(source), setSrc, Bind(..) )
import qualified Syntax.Type                   as T
import           Validation.Terminated          ( terminated )
import           Validation.Substitution        ( subs )
import           Util.Error                     ( internalError )
import           Util.KeepSrc

normalise :: T.Type -> T.Type
normalise (T.Semi p t u)
  | terminated t = normalise u
  | otherwise    = append p (normalise t) u
normalise u@(T.Rec _ (Bind _ x _ t)) = subs u x (normalise t)
normalise t@T.Dualof{} = internalError "Equivalence.Normalisation.normalise" t
normalise t = t

append :: Span -> T.Type -> T.Type -> T.Type
append _ t               (T.Skip _) = t
append p (T.End s)       _          = setSrc (source s) $ T.End p
append p (T.Semi p1 t u) v          = T.Semi p1 t (append p u v)
append p t               u          = keepSrc $ T.Semi p t u
