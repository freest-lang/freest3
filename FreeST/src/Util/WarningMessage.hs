{-# LANGUAGE FlexibleInstances, GADTs #-}

module Util.WarningMessage
    ( WarningMessage(..)
    , Color(..)
    , WarningMsg(..)
    ) where

import           Syntax.Program                 ( TypeOpsEnv )

-- | Warning class and instances

class WarningMsg a where
  msg   :: TypeOpsEnv -> a -> String
  color :: a -> Maybe Color

data WarningMessage where
  Warning :: WarningMsg a => a -> WarningMessage

data Color = Red | Pink

-- | WarningMessage instances

instance WarningMsg String where
  msg _ s = s
  color _ = Nothing

instance WarningMsg Int where
  msg _ = show
  color _ = Just Pink
