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
import Data.Char (isDigit)

data TypeVar = TypeVar Pos String

instance Variable TypeVar where
  mkVar = TypeVar
  mkNewVar next (TypeVar pos id) = TypeVar pos (show next ++ '_' : id)
  intern (TypeVar _ x) = x

instance Eq TypeVar where
  (TypeVar _ x) == (TypeVar _ y) = x == y
  
instance Ord TypeVar where
  (TypeVar _ x) <= (TypeVar _ y) = x <= y

-- instance Show TypeVar where
--   show (TypeVar _ x) = x -- showVar x
--     where
--       showVar :: String -> String
--       showVar id
--         | isDigit (head id) = tail $ dropWhile (isDigit) id
--         | otherwise         = id

instance Position TypeVar where
  position (TypeVar p _) = p
