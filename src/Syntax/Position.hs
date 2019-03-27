{- |
Module      :  Position
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Syntax.Position
( Var
, Bind(..)
) where 

import Parse.Lexer (Position, Pos, position)

type Var = String

-- Bindings

data Bind = Bind Pos Var

instance Position Bind where
  position (Bind p _) = p

instance Show Bind where
  show (Bind _ x) = x

instance Eq Bind where
  (Bind _ x) == (Bind _ y) = x == y
  
instance Ord Bind where
  (Bind _ x) `compare` (Bind _ y) = x `compare` y
