{- |
Module      :  Validation.Normalisation
Description :  Unfold recursive types and terminated types
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pot
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Unfold recursive types and get rid of terminated types, in such a way that the
top level constructor of the resulting type is not a rec and not Skip;T.
-}

module Validation.Normalisation
  ( normalise
  )
where

import           Syntax.Base                    ( Span, Bind(..) )
import qualified Syntax.Type                   as T
import           Validation.Terminated          ( terminated )
import           Validation.Substitution        ( subs )
import           Util.Error                     ( internalError )

-- TACAS 2020
normalise :: T.Type -> T.Type
normalise (T.Semi p t u)
  | terminated t = normalise u
  | otherwise    = append p (normalise t) u
normalise u@(T.Rec _ (Bind _ x _ t)) = subs u x (normalise t)
normalise t@T.Dualof{} = internalError "Validation.Normalisation.normalise" t
normalise t = t

-- Subtyping
-- normalise  :: T.Type -> T.Type 
-- normalise u@(T.Rec _ (Bind _ x _ t)) = normalise . subs u x $ t
-- normalise (T.Semi p (T.End _) u) = T.End p 
-- normalise (T.Semi p (T.Labelled _ (T.Choice v) tm) u) = T.Labelled p (T.Choice v) $ Map.map (\t -> T.Semi p t u) tm 
-- normalise (T.Semi p (T.Skip _) t) = normalise t 
-- normalise (T.Semi p u@(T.Rec _ (Bind _ x _ t)) v) = normalise $ T.Semi p (subs u x t) v
-- normalise (T.Semi p (T.Semi _ t u) v) = T.Semi p t $ T.Semi p u v -- are spans important here?
-- normalise t = t 

append :: Span -> T.Type -> T.Type -> T.Type
append _ t               (T.Skip _) = t
append p (T.End _ pol)   _          = T.End p pol
append p (T.Semi p1 t u) v          = T.Semi p1 t (append p u v)
append p t               u          = T.Semi p t u
