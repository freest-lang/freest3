{-# LANGUAGE FlexibleInstances, GADTs #-}

module Utils.ErrorMessage
  ( ErrorMessage(..)
  , Color(..)
  , ErrorMsg(..)
  )
where

import           Data.List                      ( intercalate )
import           Syntax.Base
import           Syntax.Expression
import           Syntax.Kind
import           Syntax.ProgramVariable
-- import           Syntax.Schemes
import           Syntax.TypeVariable
import qualified Syntax.Type                   as T
-- import System.Console.Pretty (Color(..))
import           Utils.ShowDefault

-- | Error class and instances

class ErrorMsg a where
--  pos   :: a -> Pos -- Does not make sense to be here??
  msg   :: T.TypeOpsEnv -> a -> String
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

instance ErrorMsg Kind where
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

showTypeList :: T.TypeOpsEnv -> [T.Type] -> String
showTypeList tops ts = "[" ++ intercalate ", " types ++ "]"
  where types = map (showDefault tops) ts

instance ErrorMsg [KindBind] where
  msg _ = show
  color _ = Just Red


-- TODO: DIFFS
-- TODO: tops
instance ErrorMsg T.VarEnv where
  msg _ = show
  color _ = Just Red
