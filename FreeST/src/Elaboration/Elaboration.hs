{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase, FlexibleInstances, BlockArguments #-}

module Elaboration.Elaboration
  ( elaboration
  , Elaboration(..)
  -- TEST INFERENCE
  , fixConsTypes
  , Match.checkNumArgs
  , Match.checkChanVar
  , Match.addMissingVars
  , Match.matchFuns
  , solveEquations
  , Dual.resolve
  , elabPEnv
  , elabVEnv
  , buildProg
  , inferKinds
  )
where

import           Elaboration.Elaborate
import           Elaboration.Inference.ConstraintGen
import           Elaboration.Inference.ResolveConstraint
import qualified Elaboration.Match as Match
import           Elaboration.ResolveDuality as Dual
import           Elaboration.ResolveEquations
import           Equivalence.Normalisation ( normalise )
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.MkName
import           Syntax.Program ( VarEnv, isDatatypeContructor, isDatatype )
import qualified Syntax.Type as T
import           Util.Error
import           Util.FreestState
import qualified Validation.Extract as Extract
import           Validation.Kinding (synthetise)
import           Validation.Rename (renameState)
import qualified Validation.Subkind as SK (join)
import           Validation.Substitution
import           Validation.Typing hiding (synthetise)

import           Control.Monad (when)
import           Control.Monad.State
import           Data.Bifunctor as Bifunctor
import           Data.Char (isLower)
import           Data.Functor
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Syntax.Base as T
import           Validation.Substitution (free)


elaboration :: FreestState ()
elaboration = do
  -- | Fix the multiplicity of the data constructor types
  fixConsTypes
  
  -- | Checks if there are choices with the same name as constructors
  --  Match.checkChoices =<< getPEnvChoices
  -- | Checks correct number of arguments
  Match.checkNumArgs =<< getPEnvPat
  -- | Checks correct channels' pattern matching
  Match.checkChanVar =<< getPEnvPat
  -- | Adds missing Vars to malformed functions
  getPEnvPat >>= setPEnvPat . Match.addMissingVars
  -- | Remove all patterns
  (Match.matchFuns =<< getPEnvPat) >>= setPEnv
  -- | Solve the equations' system.
--  debugM . ("EqsI " ++) <$> show =<< getTEnv
  solveEquations
  inferKindsOnTypes
  -- | From this point, there are no type names on the function signatures
  --   and on the function bodies. 
  -- | Then, resolve all the dualof occurrences on:
  -- | Type Env (i.e. type A = dualof !Int)
--  debugM . ("TEnvI " ++) <$> show =<< getTEnv
  (Dual.resolve =<< getTEnv) >>= setTEnv
  -- | From this point, there are no type names on the RHS
  --   of the type declarations and datatypes (type env)
  -- | Substitute all type names on the function signatures
  elabVEnv =<< getVEnv
  -- | same for parse env (which contains the functions' bodies)
  elabPEnv =<< getPEnv
  -- | Var Env (i.e. f : dualof !Int -> Skip)
  (Dual.resolve =<< getVEnv) >>= setVEnv
  -- | Parse Env (i.e. f c = send 5 c)
  (Dual.resolve =<< getPEnv) >>= setPEnv
  -- | From this point there are no more occurrences of the dualof operator
  -- | Build the expression environment: substitute all
  --   type operators on ExpEnv;
  --   From f x = E and f : T -> U
  --   build a lambda expression: f = \x : T -> E
  buildProg
  -- debugM . ("Program " ++) <$> show =<< getProg
  -- debugM . ("TypeEnv " ++) <$> show =<< getTEnv
  -- debugM . ("VenvI " ++) <$> show . Map.filterWithKey(\k _ -> k == mkVar defaultSpan "UnS" ||
  --                                                             k == mkVar defaultSpan "UnR"
  --                                                    ) =<< getVEnv
      
inferKindsOnTypes :: FreestState ()
inferKindsOnTypes = do
  s <- get
  tMapWithKeyM_ (\v (k,t) -> when (userDefined s v) $ do
                    when (isKVar k) (let (K.KindVar _ kvar) = k in addKVariable kvar)
--                    debugM $ show v ++ " : " ++ show k
                    void $ cg Map.empty t) =<< getTEnv
  c <- getConstraints
--  debugM $ "Constraints: " ++ show c
  (subsk, subsm, _) <- infer
--  debugM $ "SOLUTION: " ++ show (subsk, subsm)
  (Map.map (\(k,t) -> (test k subsk, subsOnKT t subsk)) <$> getTEnv) >>= setTEnv
  setConstraints []
  setKVariables Set.empty
  setPKVariables Set.empty
  setMVariables Set.empty
  where     -- TODO: change name
    test k@(K.KindVar _ x) subs = Map.findWithDefault k x subs
    test k _ = k

inferKinds :: FreestState () 
inferKinds = do
  s <- get  
  tMapWithKeyM_ (\k t -> when (userDefined s k) $ void $ cg Map.empty t) =<< getVEnv
  tMapWithKeyM_ (\k e -> when (userDefined s k) $ void $ infGen Map.empty e) =<< getProg
  setVEnv (varEnv s)
  (subsk, subsm, subsp) <- infer  
  (Map.map (flip subsOnKE subsk) <$> getProg) >>= setProg
  (Map.map (flip subsOnKT subsk) <$> getVEnv) >>= setVEnv
  setConstraints []
  setKVariables Set.empty
  setPKVariables Set.empty
  setMVariables Set.empty

-- NEW

subsOnKE :: E.Exp -> SubsK -> E.Exp
subsOnKE = Map.foldlWithKey subsKE

subsOnKT :: T.Type -> SubsK -> T.Type
subsOnKT = Map.foldlWithKey subsKE

class SubsKE a where
  subsKE :: a -> Variable -> K.Kind -> a

instance SubsKE E.Exp where
  subsKE (E.TypeAbs p (Bind b x v e)) kv k
    | isKVar v && kv == fromKToVar v =
        E.TypeAbs p (Bind b x k (subsKE e kv k))
    | otherwise =
        E.TypeAbs p (Bind b x v (subsKE e kv k))
  subsKE (E.Abs p m (Bind b x t e)) kv k =
    E.Abs p m (Bind b x (subsKE t kv k) (subsKE e kv k))
  subsKE (E.UnLet s x e1 e2     ) kv k = 
    E.UnLet s x (subsKE e1 kv k) (subsKE e2 kv k)
  subsKE (E.App p e1 e2          ) kv k =
    E.App p (subsKE e1 kv k) (subsKE e2 kv k)
  subsKE (E.Cond p e1 e2 e3       ) kv k =
    E.Cond p  (subsKE e1 kv k) (subsKE e2 kv k) (subsKE e3 kv k)
  subsKE (E.Pair p e1 e2         ) kv k =
    E.Pair p  (subsKE e1 kv k) (subsKE e2 kv k)
  subsKE (E.BinLet p x y e1 e2   ) kv k =
    E.BinLet p x y  (subsKE e1 kv k) (subsKE e2 kv k)
  subsKE (E.Case  p e fm        ) kv k =
    E.Case  p e (Map.map (Bifunctor.second (\e -> subsKE e kv k)) fm)
  subsKE (E.TypeApp  p e t        ) kv k =
    E.TypeApp  p (subsKE e kv k) (subsKE t kv k)
  subsKE e _ _ = e

instance SubsKE T.Type where
  subsKE (T.Forall p (Bind b x v t)) kv k
    | isKVar v && kv == fromKToVar v = T.Forall p (Bind b x k (subsKE t kv k))
    | otherwise                      =
--      trace ("\nv: " ++ show v ++ "\n"++ show kv ++ "\n") $
      T.Forall p (Bind b x v (subsKE t kv k))
  subsKE (T.Arrow p m t1 t2) kv k = T.Arrow p m (subsKE t1 kv k) (subsKE t2 kv k)
  subsKE (T.Labelled p s m) kv k = T.Labelled p s (Map.map (\t -> subsKE t kv k) m)
  subsKE (T.Semi p t1 t2   ) kv k = T.Semi p (subsKE t1 kv k) (subsKE t2 kv k)
  subsKE (T.Message p pol t) kv k = T.Message p pol (subsKE t kv k)
  subsKE (T.Rec p (Bind b x v t)) kv k
    | isKVar v && kv == fromKToVar v = T.Rec p (Bind b x k (subsKE t kv k))
    | otherwise                      =     
        T.Rec p (Bind b x v (subsKE t kv k))
  subsKE (T.Dualof p t   ) kv k = T.Dualof p (subsKE t kv k)
  subsKE t _ _  = t
                              


------------------------------------------------------------
  

-- | Fix the multiplicity of the data constructor types
fixConsTypes :: FreestState ()
fixConsTypes = do
  tEnv <- getTEnv
  -- if this is the first step in the elaboration, there are still type names in signatures,
  -- so we need a non-empty kind environment. Empty env otherwise.
  let kEnv = Map.map fst tEnv
  getVEnv >>= tMapWithKeyM_ \k v -> when (isDatatypeContructor k tEnv)
    (fixConsType kEnv K.Un v >>= addToVEnv k)
  where
    fixConsType :: K.KindEnv -> K.Multiplicity -> T.Type -> FreestState T.Type
    fixConsType kEnv m t'@(T.Arrow s _ t u) = do
      synthetise kEnv t >>= \case
        (K.Kind _ m' _) -> 
          T.Arrow s (kindToTypeMult m) t <$> fixConsType kEnv (SK.join m m') u
        (K.KindVar p _) ->
          T.Arrow s (kindToTypeMult m) t <$> fixConsType kEnv m u
      where kindToTypeMult K.Un = Un
            kindToTypeMult K.Lin = Lin
    fixConsType _ _ t = pure t

-- | Elaboration over environments (VarEnv + ParseEnv)

elabVEnv :: VarEnv -> FreestState ()
elabVEnv = tMapWithKeyM_ (\pv t -> addToVEnv pv . quantifyLowerFreeVars =<< elaborate t)
  where quantifyLowerFreeVars t = 
          foldr (\v t -> T.Forall p (T.Bind p v (K.ut p) t)) t (Set.filter (isLower.head.show) $ free t)
          where p = getSpan t

elabPEnv :: ParseEnv -> FreestState ()
elabPEnv = tMapWithKeyM_ (\x (ps, e) -> addToPEnv x ps =<< elaborate e)

-- | Build a program from the parse env

buildProg :: FreestState ()
buildProg = getPEnv
  >>= tMapWithKeyM_ (\pv (ps, e) -> addToProg pv =<< buildFunBody pv ps e)

buildFunBody :: Variable -> [Variable] -> E.Exp -> FreestState E.Exp
buildFunBody f as e = getFromVEnv f >>= \case
    Just s  -> buildExp e as s
    Nothing -> addError (FuctionLacksSignature (getSpan f) f) $> e
 where
  buildExp :: E.Exp -> [Variable] -> T.Type -> FreestState E.Exp
  buildExp e [] _ = pure e
  buildExp e bs t@(T.Rec _ _) = buildExp e bs (normalise t)
  buildExp e (b : bs) (T.Arrow _ m t1 t2) =
    E.Abs (getSpan b) m . Bind (getSpan b) b t1 <$> buildExp e bs t2
  buildExp e bs (T.Forall p (Bind p1 x k t)) =
    E.TypeAbs p . Bind p1 x k <$> buildExp e bs t
  buildExp _ _ t@(T.Dualof _ _) = internalError "Elaboration.Elaboration.buildFunbody.buildExp" t
  buildExp _ xs _ = do
    t <- fromJust <$> getFromVEnv f
    addError (WrongNumberOfArguments (getSpan f) f (length as - length xs) (length as) t) $> e
