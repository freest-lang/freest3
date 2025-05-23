{- |
Module      :  Syntax.TypeVariables
Description :  The type variables
Copyright   :  (c) Bernardo Almeida, LASIGE, Faculty of Sciences, University of Lisbon
                   Andreia Mordido, LASIGE, Faculty of Sciences, University of Lisbon
                   Vasco Vasconcelos, LASIGE, Faculty of Sciences, University of Lisbon
Maintainer  :  balmeida@lasige.di.fc.ul.pt, afmordido@fc.ul.pt, vmvasconcelos@fc.ul.pt
-}

module Syntax.TypeVariables
( TypeVar
) where

import Syntax.Base

data TypeVar = TypeVar Pos String

instance Variable TypeVar where
  mkVar = TypeVar
  mkNewVar next (TypeVar pos str) = TypeVar pos (show next ++ '#' : str)
  intern (TypeVar _ x) = x

instance Eq TypeVar where
  (TypeVar _ x) == (TypeVar _ y) = x == y
  
instance Ord TypeVar where
  (TypeVar _ x) <= (TypeVar _ y) = x <= y

instance Position TypeVar where
  position (TypeVar p _) = p
