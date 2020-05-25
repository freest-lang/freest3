{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
module Utils.ErrorMessage (ErrorMessage(..), Color(..), ErrorMsg(..)) where

import Data.List (intercalate)
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

-- VarEnv

instance ErrorMsg Int where
  pos   _ = defaultPos
  msg   _ = show
  color _ = Just Red      

instance ErrorMsg [Type] where
  pos _ = defaultPos
  msg tops ts = showTypeList tops ts
  color _ = Just Red

showTypeList :: TypeOpsEnv -> [Type] -> String
showTypeList tops ts = "[" ++ intercalate ", " types ++ "]"
  where types = map (showDefault tops) ts
  
instance ErrorMsg [TypeVarBind] where
  pos   _ = defaultPos
  msg _   = show
  color _ = Just Red
  

-- TODO: DIFFS
-- TODO: tops
instance ErrorMsg VarEnv where
  pos _   = defaultPos
  msg _   = show
  color _ = Just Red
