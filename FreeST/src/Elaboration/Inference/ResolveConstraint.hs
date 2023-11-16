{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Elaboration.Inference.ResolveConstraint where

import           Control.Monad.State.Lazy hiding (join)
import           Data.Bifunctor as Bifunctor
import           Data.Functor
import           Data.List
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Data.Traversable (traverse)
import qualified Data.Traversable as Traversable
import           Debug.Trace
-- import           Elaboration.Elaboration
-- import           Elaboration.InfState
import           Parse.ParseUtils
import           Parse.Parser ( parseProgram, parseAndImport )
import           Parse.Read
import           Paths_FreeST ( getDataFileName )
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.MkName
import           Syntax.Program
import qualified Syntax.Type as T
import           System.IO
import           Util.FreestState -- (tMapM_,tMapM, initialState, FreestS(..), RunOpts(..))
import qualified Validation.Extract as Extract
import           Validation.Rename -- hiding (subs)
import qualified Validation.Subkind as SK
-- import           Validation.Substitution
import           Validation.Typing
import Validation.Kinding (typeToKindMult)

------------------------------------------------------------
-- RESOLVE CONSTRAINTS
------------------------------------------------------------


type SubsK = Map.Map Variable K.Kind
type SubsM = Map.Map Variable K.Multiplicity
type SubsPK = Map.Map Variable K.PreKind

topKind = K.lt defaultSpan
topPreKind = K.Top
topMult = K.Lin

infer :: FreestState (SubsK, SubsM, SubsPK)
infer = do
  -- kc <- getKConstraints
  -- mc <- getMConstraints
  -- vk <- getKVariables
  -- vm <- getMVariables
--  let joinCtxs = map Left kc ++ map Right mc
  subsK <- initialSubs topKind  getKVariables
  subsM <- initialSubs topMult getMVariables
  subsPK <- initialSubs topPreKind getPKVariables
--  error $ show subsK ++ "\n" ++ show subsM
  res@(a,b,c) <- inferAll (subsK, subsM, subsPK) =<< getConstraints
  -- debugM . show =<< getConstraints
  -- debugM $ show a
  return res
 where
   initialSubs :: a -> FreestState (Set.Set Variable) -> FreestState (Map.Map Variable a)
   initialSubs top getVar = Set.foldl (\acc k -> Map.insert k top acc) Map.empty <$> getVar

inferAll :: (SubsK, SubsM, SubsPK) -> [Constraint] -> FreestState (SubsK, SubsM, SubsPK)
inferAll s cs = do
  res@(sk,_,_) <- inferAll' s cs
  let erros = foldl (\acc e -> case e of
                        MultC _ -> acc
                        KindP _ -> acc
                        KindC (k1,k2)
                          | isKVar k1 && isKVar k2 -> cmp (sk Map.! fromKToVar k1) (sk Map.! fromKToVar k2) acc
                          | isKVar k1 -> cmp (sk Map.! fromKToVar k1) k2 acc                              
                          | isKVar k2 -> cmp k1 (sk Map.! fromKToVar k2) acc
                          | otherwise -> acc
                        
                    ) [] cs
  -- unless (null erros) (error $ show erros)
  return res
  where
    cmp k1 k2 acc
      | k1 SK.<: k2 = acc
      | otherwise   = (show k1 ++ " <: " ++ show k2) : acc
  
    inferAll' :: (SubsK, SubsM, SubsPK) -> [Constraint] -> FreestState (SubsK, SubsM, SubsPK)
    inferAll' s cs = do
--      debugM $ "Ciclo: " ++ show (length cs)
      !(s', cs') <- foldM inferOne (s, cs) cs
      if s == s' then pure s' else inferAll' s' cs'

-- inferOne :: (SubsK, SubsM) -> Either KindConstraint MultConstraint -> FreestState (SubsK, SubsM)
inferOne :: ((SubsK, SubsM, SubsPK), [Constraint]) -> Constraint -> FreestState ((SubsK, SubsM, SubsPK), [Constraint])
inferOne ((sk, sm, spk), cs) c@(KindC (k1,k2))
  | isKVar k1 = do
      let var = fromKToVar k1
      let k2' = getFromSubs k2
      let k3 = sk Map.! var
      
      -- debugM (show k1 ++ " <: " ++ show k2 ++   "\n     "
      --      ++ show var ++ " = " ++ show k3 ++   "\n     "
      --      ++ show var ++ " = " ++ show k2' ++ " ⊓ " ++ show k3 ++ " = "
      --      ++ show (SK.meet k2' k3))
      let cs' = if properKind k2 then delete c cs else cs
      return ((Map.insert var (SK.meet k2' k3) sk, sm, spk), cs')
  | isKVar k2 = return ((sk, sm, spk), cs)
  | otherwise =  -- return (sk, sm, spk)
      let cs' = delete c cs in
      if k1 SK.<: k2
        then return ((sk, sm, spk), cs')
        else -- error ("Contraint infraction2: " ++ show k1 ++ " is not a subkind (<:) of " ++ show k2) --
             return ((sk, sm, spk), cs') -- 
 where
   getFromSubs (K.KindVar _ k) = sk Map.! k
   getFromSubs (K.Kind s (K.MultVar m) p) = K.Kind s (sm Map.! m) p
   getFromSubs k = k
  
inferOne ((sk, sm, spk), cs) m@(MultC (mv,ks)) =
  let lub = foldl upperBound K.Un (map getMults ks) in
 -- debugM (show m ++ " " ++ show lub) >>
  let cs' = if all properKind ks then delete m cs  else cs in 
  
  return ((sk, Map.insert (fromMToVar mv) lub sm, spk), cs')
  where
    getMults k
      | isKVar k  = mult $ sk Map.! fromKToVar k
      | otherwise = mult k
      
    upperBound currLeast m
      | isMVar m  = SK.join (sm Map.! fromMToVar m) currLeast
      | otherwise = SK.join m currLeast

inferOne ((sk, sm, spk), cs) c@(KindP (K.PreKindVar pkv,ks)) = do
--  let curr = spk Map.! pkv
  let glb = foldl (\curr k -> prekind (getFromVar k) `SK.meet` curr) (spk Map.! pkv) ks
  -- let glb = prekind (getFromVar k1) `SK.meet` prekind (getFromVar k2) `SK.meet` curr

  let cs' = if all properKind ks then delete c cs else cs

  return ((sk,sm, Map.insert pkv glb spk), cs')
  where
    getFromVar :: K.Kind -> K.Kind
    getFromVar (K.KindVar _ k) = sk Map.! k
    getFromVar k@(K.Kind s (K.MultVar m) (K.PreKindVar v)) = K.Kind s (sm Map.! m) (spk Map.! v)
    getFromVar k@(K.Kind s (K.MultVar m) v) = K.Kind s (sm Map.! m) v
    getFromVar k@(K.Kind s m (K.PreKindVar v)) = K.Kind s m (spk Map.! v)
    getFromVar k = k





isProperKind :: K.Kind -> Bool
isProperKind K.KindVar{} = False
isProperKind (K.Kind _ K.MultVar{} _) = False
isProperKind (K.Kind _ _ K.PreKindVar{}) = False
isProperKind _ = True
    
-- -- Alter


cleanConstK :: [KindConstraint] -> KindConstraint -> [KindConstraint]
cleanConstK acc (K.Kind _ K.Lin K.Session, K.Kind _ K.Un K.Top) = acc
cleanConstK acc ks@(k1,k2)
  | properKind k1 && properKind k2 =
      if k1 SK.<: k2 then acc else
       traceM ("Constraint, proper error " ++ show k1 ++ " <: " ++ show k2) >> ks:acc
  | otherwise = ks:acc
    
properKind :: K.Kind -> Bool
properKind (K.KindVar _ _) = False
properKind (K.Kind _ K.MultVar{} _) = False
properKind _ = True

cleanConstM :: [MultConstraint] -> MultConstraint -> [MultConstraint]
cleanConstM acc (_,ks) | all (\case {(K.Kind _ m _) -> m == K.Lin; _ -> False}) ks = acc
cleanConstM acc ks = ks:acc


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

prekind :: K.Kind -> K.PreKind
prekind (K.Kind _ _ v) = v
prekind _ = undefined

isKVar :: K.Kind -> Bool
isKVar K.KindVar{} = True
isKVar _           = False

fromKToVar :: K.Kind -> Variable
fromKToVar (K.KindVar _ v) = v
fromKToVar v             = error $ "undefined: "++ show v

isMVar :: K.Multiplicity -> Bool
isMVar K.MultVar{} = True
isMVar _           = False

isPKVar :: K.PreKind -> Bool
isPKVar K.PreKindVar{} = True
isPKVar _              = False

fromMToVar :: K.Multiplicity -> Variable
fromMToVar (K.MultVar v) = v
fromMToVar _             = undefined

