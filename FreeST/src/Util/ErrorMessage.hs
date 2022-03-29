{-# LANGUAGE FlexibleInstances, GADTs #-}

module Util.ErrorMessage
  ( ErrorMessage(..)
  , Color(..)
  , ErrorMsg(..)
  )
where

import           Syntax.Base
import           Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import           Syntax.Program
import qualified Syntax.Type                   as T
import           Parse.Unparser
import           Util.GetTOps
import           Data.List                      ( intercalate )

-- | Error class and instances

class ErrorMsg a where
  msg   :: TypeOpsEnv -> a -> String
  color :: a -> Maybe Color

data ErrorMessage where
  Error :: ErrorMsg a => a -> ErrorMessage

data Color = Red | Cyan

-- | ErrorMessage instances

instance ErrorMsg T.Type where
  msg tops t = show $ getDefault tops t
  color _ = Just Red

instance ErrorMsg String where
  msg _ s = s
  color _ = Nothing

instance ErrorMsg E.Exp where
  msg tops e = show $ getDefault tops e
  color _ = Just Red

instance ErrorMsg Variable where
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
  where types = map (show . getDefault tops) ts

instance ErrorMsg [Bind K.Kind T.Type] where
  msg _ = concatMap showBindType
  color _ = Just Red

instance ErrorMsg [Bind K.Kind E.Exp] where
  msg _ = concatMap showBindExp
  color _ = Just Red

-- TODO: Diffs, Tops ???
instance ErrorMsg VarEnv where
  msg _ = show
  color _ = Just Red
