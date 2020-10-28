{-# LANGUAGE FlexibleInstances, GADTs #-}

module Utils.ErrorMessage
  ( ErrorMessage(..)
  , Color(..)
  , ErrorMsg(..)
  )
where

import           Data.List                      ( intercalate )
import           Syntax.Base
import           Syntax.Expressions
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.Schemes
import           Syntax.TypeVariables
import           Syntax.Types
-- import System.Console.Pretty (Color(..))
import           Utils.ShowDefault

-- | Error class and instances

class ErrorMsg a where
--  pos   :: a -> Pos -- Does not make sense to be here??
  msg   :: TypeOpsEnv -> a -> String
  color :: a -> Maybe Color

data ErrorMessage where
  Error ::ErrorMsg a => a -> ErrorMessage

data Color = Red

-- | ErrorMessage instances

instance ErrorMsg Type where
  msg = showDefault
  color _ = Just Red

instance ErrorMsg String where
  msg _ s = s
  color _ = Nothing

instance ErrorMsg Expression where
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

instance ErrorMsg Kind where
  msg _ = show
  color _ = Just Red

instance ErrorMsg TypeScheme where
  msg _ = show
  color _ = Just Red

-- VarEnv

instance ErrorMsg Int where

  msg _ = show
  color _ = Just Red

instance ErrorMsg [Type] where
  msg = showTypeList
  color _ = Just Red

showTypeList :: TypeOpsEnv -> [Type] -> String
showTypeList tops ts = "[" ++ intercalate ", " types ++ "]"
  where types = map (showDefault tops) ts

instance ErrorMsg [KindBind] where
  msg _ = show
  color _ = Just Red


-- TODO: DIFFS
-- TODO: tops
instance ErrorMsg VarEnv where
  msg _ = show
  color _ = Just Red
