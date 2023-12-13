{- |
Module      :  Typing.Normalisation
Description :  Unfold recursive types and terminated types
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pot
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Unfold recursive types and get rid of terminated types, in such a way that the
top level constructor of the resulting type is not a rec and not Skip;T.
-}

module Typing.Normalisation
  ( normalise
  )
where

import           Syntax.Base                ( Span, Bind(..) )
import qualified Syntax.Type                 as T
import           Typing.Substitution     ( subs )
import           Kinding.Terminated          ( terminated )
import           Util.Error                  ( internalError )

normalise :: T.Type -> T.Type
normalise (T.Semi p t u)
  | terminated t = normalise u
  | otherwise    = append p (normalise t) u
normalise u@(T.Rec _ (Bind _ x _ t)) = subs u x (normalise t)
normalise t@T.Dualof{} = internalError "Typing.Normalisation.normalise" t
normalise t = t

append :: Span -> T.Type -> T.Type -> T.Type
append _ t               (T.Skip _) = t
append p (T.End _ pol)   _          = T.End p pol
append p (T.Semi p1 t u) v          = T.Semi p1 t (append p u v)
append p t               u          = T.Semi p t u
