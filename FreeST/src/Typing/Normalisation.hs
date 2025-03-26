{- |
Module      :  Typing.Normalisation
Description :  Normalise a session type
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pot
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

Normalise a session type by

- Unfolding recursive types
- Getting rid of terminated types
- Tame the associativity of semicolon
- Turn message and choice types T into T;Skip

in such a way that the resulting type
- is T;U where T is !, ?, + or &,
- is never a recursive type (rec types are unfolded), or
- is the original type

top level constructor of the resulting type is either not a rec
and not Skip;T.
-}

module Typing.Normalisation
  ( normalise
  )
where

import           Syntax.Base         ( Span, Bind(..) )
import qualified Syntax.Type         as T
import           Kinding.Terminated  ( terminated )
import           Typing.Substitution ( subs )
import           Util.Error          ( internalError )


normalise :: T.Type -> T.Type
  -- Session Types
normalise (T.Semi p t u)
  | terminated t = normalise u
  | otherwise    = append p (normalise t) u
normalise (T.Message s l p t) = T.Semi s (T.Message s l p t) (T.Skip s)
normalise (T.Labelled s c@T.Choice{} l m) = T.Semi s (T.Labelled s c l m) (T.Skip s)
  -- recursive types
normalise u@(T.Rec _ (Bind _ x _ t)) = subs u x (normalise t)
normalise t@T.Dualof{} = internalError "Typing.Normalisation.normalise" t
normalise t = t

append :: Span -> T.Type -> T.Type -> T.Type
append _ t               (T.Skip _) = t
append p (T.End _ pol)   _          = T.End p pol
append p (T.Semi p1 t u) v          = T.Semi p1 t (append p u v)
append p t               u          = T.Semi p t u
