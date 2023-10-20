module Elaboration.InfState where



import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.MkName
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.FreestState hiding
  (kConstraints, mConstraints, mVariables, kVariables, addKVariable, getKConstraints, addMVariable)
  
import qualified Validation.Subkind as SK
import           Validation.Substitution
import qualified Validation.Extract as Extract

import           Control.Monad.State.Lazy hiding (join)
import           Data.Functor
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Data.Traversable (traverse)
import qualified Data.Traversable as Traversable

import Validation.Typing

-- TODO: test
import           Parse.Read
import           Validation.Rename hiding (subs)
import           Parse.ParseUtils
import           Debug.Trace
import           System.IO
import Data.Bifunctor as Bifunctor
import           Paths_FreeST ( getDataFileName )
--import           Util.CmdLine
import           Parse.Parser ( parseProgram, parseAndImport )
import Elaboration.Elaboration
import Data.Maybe
import Data.List



-- type KindConstraint = (K.Kind, K.Kind)
-- type MultConstraint = (K.Multiplicity, [K.Kind]) 

-- type KindConstraints = [KindConstraint]
-- type MultConstraints = [MultConstraint]


data InferS = InferS { index ::  Int
                     , kConstraints :: KindConstraints
                     , mConstraints :: MultConstraints
                     , kVariables :: Set.Set Variable
                     , mVariables :: Set.Set Variable
                     , vEnv       :: VarEnv
                     , bindVarKind :: Map.Map Variable K.Kind -- TODO: test remove afterwards
                     } deriving Show

initial :: InferS
initial = InferS { index  = 0
                 , kConstraints = []
                 , mConstraints = []
                 , kVariables   = Set.empty
                 , mVariables   = Set.empty
                 , vEnv         = Map.empty
                 , bindVarKind  = Map.empty
                 }


addBind :: Variable -> K.Kind -> InferState ()          
addBind v k = modify (\s -> s {bindVarKind = Map.insert v k (bindVarKind s)} )

getBind :: InferState (Map.Map Variable K.Kind)          
getBind = gets bindVarKind

-- getBindOrNew :: Variable -> InferState K.Kind
-- getBindOrNew v = do
--   m <- map (first show) . Map.toList <$> getBind
--   case lookup (show v) m of
--     Just k -> return k
--     Nothing -> do
-- --      traceM $ ">>>>>>>>>>>>>>> "++ show v ++ " " ++ show m
--       kv <- freshKindVar
--       addBind v kv
--       return kv
  

getIndex :: InferState Int
getIndex = do
  next <- gets index
  modify (\s -> s { index = next + 1 })
  return next

freshKindVar :: Span -> InferState K.Kind
freshKindVar s = do
  i <- getIndex
  return $ K.KindVar s $ mkVar defaultSpan ("χ" ++ show i )

freshMultVar :: InferState K.Multiplicity
freshMultVar = do
  i <- getIndex
  let v = mkVar defaultSpan ("φ" ++ show i )
  addMVariable v
  return $ K.MultVar v

-- fromMVar :: K.Multiplicity -> Variable
-- fromMVar (K.MultVar v) = v
-- fromMVar _             = undefined


getKConstraints :: InferState KindConstraints
getKConstraints = gets kConstraints

getMConstraints :: InferState MultConstraints
getMConstraints = gets mConstraints

getKVariables :: InferState (Set.Set Variable)
getKVariables = gets kVariables

getMVariables :: InferState (Set.Set Variable)
getMVariables = gets mVariables

getVarEnv :: InferState VarEnv
getVarEnv = gets vEnv

addToVarEnv ::  Variable -> T.Type -> InferState ()
addToVarEnv k v =
  modify (\s -> s { vEnv = Map.insert k v (vEnv s) })

rmFromVarEnv ::  Variable -> InferState ()
rmFromVarEnv k =
  modify (\s -> s { vEnv = Map.delete k (vEnv s) })

addKConstraint :: KindConstraint -> InferState ()
addKConstraint c = modify (\s -> s { kConstraints = c : kConstraints s })

getKConstForVar :: K.Kind -> InferState K.Kind
getKConstForVar x =
  foldl (\acc (y,k) -> if isKVar x && x == y then k else acc)
    (K.us defaultSpan) <$> getKConstraints

addMConstraint :: MultConstraint -> InferState ()
addMConstraint c = modify (\s -> s { mConstraints = c : mConstraints s })

addKVariable :: Variable -> InferState ()
addKVariable c = modify (\s -> s { kVariables = c `Set.insert` kVariables s })

addKVariableFromK :: K.Kind -> InferState ()
addKVariableFromK (K.KindVar _ v) = addKVariable v
addKVariableFromK _             = return ()


addMVariable :: Variable -> InferState ()
addMVariable c = modify (\s -> s { mVariables = c `Set.insert` mVariables s })

type InferState = State InferS


-- Map.map (subsOnK mv lub) sk
subsOnK :: Variable -> K.Multiplicity -> K.Kind -> K.Kind
subsOnK subsMVar lub k@(K.Kind s (K.MultVar v) p)
  | subsMVar == v = K.Kind s lub p
  | otherwise = k
subsOnK _ _ k = k

isPrekindWithMVar :: K.Kind -> Bool
isPrekindWithMVar (K.Kind _ K.MultVar{} _) = True
isPrekindWithMVar _ = False

mult :: K.Kind -> K.Multiplicity
mult (K.Kind _ m _) = m -- if isMVar m then K.Lin else m
mult _ = undefined

isKVar :: K.Kind -> Bool
isKVar K.KindVar{} = True
isKVar _           = False

fromKToVar :: K.Kind -> Variable
fromKToVar (K.KindVar _ v) = v
fromKToVar v             = error $ "undefined: "++ show v

isMVar :: K.Multiplicity -> Bool
isMVar K.MultVar{} = True
isMVar _           = False

fromMToVar :: K.Multiplicity -> Variable
fromMToVar (K.MultVar v) = v
fromMToVar _             = undefined

