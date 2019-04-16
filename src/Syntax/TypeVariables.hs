{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.TypeVariables
( TypeVar
) where

import Syntax.Base

data TypeVar = TypeVar Pos String

instance Variable TypeVar where
  mkVar = TypeVar
  mkNewVar next (TypeVar pos id) = TypeVar pos (show next ++ '_' : id)
  intern (TypeVar _ x) = x

instance Eq TypeVar where
  (TypeVar _ x) == (TypeVar _ y) = x == y
  
instance Ord TypeVar where
  (TypeVar _ x) <= (TypeVar _ y) = x <= y

instance Position TypeVar where
  position (TypeVar p _) = p
