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

module Syntax.ProgramVariables
( ProgVar
) where

import Syntax.Base

-- Note: isomorphic to TypeVariables: Type <-> Prog
data ProgVar = ProgVar Pos String

instance Variable ProgVar where
  mkVar = ProgVar
  mkNewVar next (ProgVar pos id) = ProgVar pos (show next ++ '_' : id)
  intern (ProgVar _ x) = x

instance Eq ProgVar where
  (ProgVar _ x) == (ProgVar _ y) = x == y
  
instance Ord ProgVar where
  (ProgVar _ x) <= (ProgVar _ y) = x <= y

instance Position ProgVar where
  position (ProgVar p _) = p


