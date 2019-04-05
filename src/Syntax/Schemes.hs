{- |
Module      :  Programs
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.Schemes
( TypeScheme(..)
, toTypeScheme
) where

import           Syntax.Bind
import           Syntax.Types
import           Parse.Lexer (Position, Pos, position)
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map

data TypeScheme = TypeScheme Pos [TBindK] Type

instance Show TypeScheme where
  show (TypeScheme _ [] t) = show t
  show (TypeScheme _ bs t) = "forall " ++ bindings ++ " => " ++ show t
    where bindings = concat $ intersperse ", " (map show bs)

instance Position TypeScheme where
  position (TypeScheme p _ _) = p

instance Default TypeScheme where
 omission p = TypeScheme p [] (omission p)

toTypeScheme :: Type -> TypeScheme
toTypeScheme t = TypeScheme (position t) [] t
