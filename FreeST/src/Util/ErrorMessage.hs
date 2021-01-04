{-# LANGUAGE FlexibleInstances, GADTs #-}

module Util.ErrorMessage
  ( ErrorMessage(..)
  , Color(..)
  , ErrorMsg(..)
  )
where

import           Data.List                      ( intercalate )
import           Syntax.Base
import           Syntax.Expression
import qualified Syntax.Kind                   as K
import           Syntax.Program
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           Util.ShowDefault

-- | Error class and instances

class ErrorMsg a where
--  pos   :: a -> Pos -- Does not make sense to be here??
  msg   :: TypeOpsEnv -> a -> String
  color :: a -> Maybe Color

data ErrorMessage where
  Error ::ErrorMsg a => a -> ErrorMessage

data Color = Red

-- | ErrorMessage instances

instance ErrorMsg T.Type where
  msg = showDefault
  color _ = Just Red

instance ErrorMsg String where
  msg _ s = s
  color _ = Nothing

instance ErrorMsg Exp where
  msg = showDefault
  color _ = Just Red

instance ErrorMsg ProgVar where
  msg _ = show
  color _ = Just Red

instance ErrorMsg TypeVar where
  msg _ = show
  color _ = Just Red


instance ErrorMsg Pos where
  msg _ = show
  color _ = Nothing

instance ErrorMsg K.Kind where
  msg _ = show
  color _ = Just Red

-- instance ErrorMsg TypeScheme where
--   msg _ = show
--   color _ = Just Red

-- VarEnv

instance ErrorMsg Int where

  msg _ = show
  color _ = Just Red

instance ErrorMsg [T.Type] where
  msg = showTypeList
  color _ = Just Red

showTypeList :: TypeOpsEnv -> [T.Type] -> String
showTypeList tops ts = "[" ++ intercalate ", " types ++ "]"
  where types = map (showDefault tops) ts

instance ErrorMsg [K.Bind T.Type] where
  msg _ = show
  color _ = Just Red

instance ErrorMsg [K.Bind Exp] where
  msg _ = show
  color _ = Just Red


-- TODO: DIFFS
-- TODO: tops
instance ErrorMsg VarEnv where
  msg _ = show
  color _ = Just Red
