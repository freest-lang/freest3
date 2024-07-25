module Kinding.Norm (normed) where

import           Syntax.Base
import qualified Syntax.Type as T

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

normed :: Set.Set Variable -> T.Type -> Bool
normed s (T.Semi _ t u) = normed s t && normed s u
normed s (T.Rec _ b) = normed s (body b)
normed s (T.Labelled _ (T.Choice _) m) = any (normed s) (Map.elems m)
normed s (T.Var _ a) = Set.member a s
normed _ _ = True
