module Utils.ShowM
( showTypeM
, showExpM) where

import           Syntax.Expressions
import           Syntax.Schemes
import           Syntax.Kinds
import           Syntax.Types (Type(..))
import           Syntax.TypeVariables
import           Syntax.ProgramVariables
import           Syntax.Base
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Data.List (intercalate)
-- import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import System.Console.Pretty (Color (..)) -- TODO: refactor
import Utils.FreestState

-- | Show types, consulting the typename map

showTypeM :: Type -> FreestState String
showTypeM t = liftM show (showTypeM' t)

showTypeM' :: Type -> FreestState Type
showTypeM' (Semi p t u) =
  lookupPos p (liftM2 (Semi p) (showTypeM' t) (showTypeM' u))
showTypeM' (Rec p xs t) = -- do
  lookupPos p (liftM (Rec p xs) (showTypeM' t))
showTypeM' (Fun p m t u) =
  lookupPos p (liftM2 (Fun p m) (showTypeM' t) (showTypeM' u))
showTypeM' (PairType p t u) =
  lookupPos p (liftM2 (PairType p) (showTypeM' t) (showTypeM' u))
showTypeM' (Datatype p m) = 
  lookupPos p (liftM (Datatype p) (mapM showTypeM' m))  
showTypeM' (Choice p pol m) = -- do
  lookupPos p (liftM (Choice p pol) (mapM showTypeM' m)) 
showTypeM' t = do
  tns <- getTypeNames
  return $ Map.findWithDefault t (position t) tns

lookupPos :: Pos -> FreestState Type -> FreestState Type
lookupPos p t = do
  tns <- getTypeNames
  case tns Map.!? p of
    Just u  -> pure u
    Nothing -> t

-- | Show expression, consulting the typename map
showExpM :: Expression -> FreestState String
showExpM e = liftM show (showExpM' e)

showExpM' :: Expression -> FreestState Expression
showExpM' (Lambda p m b t e) =
  liftM2 (Lambda p m b) (showTypeM' t) (showExpM' e)
showExpM' (TypeApp p x ts) =
  liftM (TypeApp p x) (mapM showTypeM' ts)
showExpM' (New p t u) =
  showTypeM' t >>= \t' -> pure $ New p t' u
showExpM' e = pure e
