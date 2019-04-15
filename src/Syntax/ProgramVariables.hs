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
import Data.Char (isDigit)

-- Note: isomorphic to TypeVariables: Type <-> Prog
data ProgVar = ProgVar Pos String

instance MkVar ProgVar where
  mkVar = ProgVar

instance MkNewVar ProgVar where
  mkNewVar next (ProgVar pos id) = ProgVar pos (show next ++ '_' : id)

instance Intern ProgVar where
  intern (ProgVar _ x) = x

instance Eq ProgVar where
  (ProgVar _ x) == (ProgVar _ y) = x == y
  
instance Ord ProgVar where
  (ProgVar _ x) <= (ProgVar _ y) = x <= y

instance Show ProgVar where
  show (ProgVar _ x) = x -- showVar x
    where
      showVar :: String -> String
      showVar id
        | isDigit (head id) = tail $ dropWhile (isDigit) id
        | otherwise         = id

instance Position ProgVar where
  position (ProgVar p _) = p


