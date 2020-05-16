{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Utils.ErrorMessage where

import Syntax.Base
import Syntax.Types
import Syntax.Schemes
import Syntax.Kinds
import Syntax.Expressions
import Syntax.ProgramVariables
import Syntax.TypeVariables
import Syntax.Show

import System.Console.Pretty (Color(..))
import           Control.Monad.State

import qualified Data.Map.Strict as Map

import           Data.List (intercalate)
import Utils.Errors
import Utils.FreestState
import Utils.ShowM

-- | The error message class is in Utils.FreestState
-- | due to cyclic imports


-- | ErrorMessage instances
  
instance ErrorMsg Type where
  pos     = position
  msg     = showTypeM
  color _ = Just Red

instance ErrorMsg String where
  pos _   = defaultPos 
  msg s   = return s
  color _ = Nothing
  
instance ErrorMsg Expression where
  pos     = position
  msg     = showExpM
  color _ = Just Red

instance ErrorMsg ProgVar where
  pos     = position
  msg     = pure . show
  color _ = Just Red

instance ErrorMsg TypeVar where
  pos     = position
  msg     = pure . show
  color _ = Just Red


instance ErrorMsg Pos where
  pos     = id
  msg     = pure . show
  color _ = Nothing

instance ErrorMsg Kind where
  pos     = position
  msg     = pure . show
  color _ = Just Red      

instance ErrorMsg TypeScheme where
  pos     = position
  msg     = pure . show
  color _ = Just Red      
