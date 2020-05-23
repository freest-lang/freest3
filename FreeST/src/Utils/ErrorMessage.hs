{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
module Utils.ErrorMessage where

import Syntax.Base
import Syntax.Expressions
import Syntax.Kinds
import Syntax.ProgramVariables
import Syntax.Schemes
import Syntax.TypeVariables
import Syntax.Types
-- import System.Console.Pretty (Color(..))
import Utils.ShowDefault

-- | Error class and instances

class ErrorMsg a where
  pos   :: a -> Pos -- Does not make sense to be here??
  msg   :: TypeOpsEnv -> a -> String
  color :: a -> Maybe Color
   
data ErrorMessage where
  Error :: ErrorMsg a => a -> ErrorMessage

data Color = Red

-- | ErrorMessage instances
  
instance ErrorMsg Type where
  pos     = position
  msg     = showDefault
  color _ = Just Red

instance ErrorMsg String where
  pos _   = defaultPos 
  msg _ s = s
  color _ = Nothing
  
instance ErrorMsg Expression where
  pos     = position
  msg     = showDefault 
  color _ = Just Red

instance ErrorMsg ProgVar where
  pos     = position
  msg   _ = show
  color _ = Just Red

instance ErrorMsg TypeVar where
  pos     = position
  msg   _ = show
  color _ = Just Red


instance ErrorMsg Pos where
  pos     = id
  msg   _ = show
  color _ = Nothing

instance ErrorMsg Kind where
  pos     = position
  msg   _ = show
  color _ = Just Red      

instance ErrorMsg TypeScheme where
  pos     = position
  msg   _ = show
  color _ = Just Red      
