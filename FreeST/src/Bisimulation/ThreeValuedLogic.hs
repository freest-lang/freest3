{- |
Module      :  Bisimulation.ThreeValuedLogic
Description :  Three-valued logic
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  vmvasconcelos@ciencias.ulisboa.pt
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

"In logic, a three-valued logic (also trinary logic, trivalent, ternary, or
trilean,[1] sometimes abbreviated 3VL) is any of several many-valued logic
systems in which there are three truth values indicating true, false and some
third value. This is contrasted with the more commonly known bivalent logics
(such as classical sentential or Boolean logic) which provide only for true and
false." https://en.wikipedia.org/wiki/Three-valued_logic
-}

module Bisimulation.ThreeValuedLogic
  ( Trinary (..)
  , (&&&)
  , fromBool
  , toBool
  )
where

import qualified Prelude

data Trinary = True | False | Unknown

fromBool :: Prelude.Bool -> Trinary
fromBool Prelude.True  = True
fromBool Prelude.False = False

toBool :: Trinary -> Prelude.Bool
toBool True = Prelude.True
toBool _    = Prelude.False

(&&&) :: Trinary -> Trinary -> Trinary
True    &&& True    = True
Unknown &&& Unknown = Unknown
True    &&& Unknown = Unknown
Unknown &&& True    = Unknown
_       &&& _       = False
