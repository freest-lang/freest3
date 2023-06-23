{-# LANGUAGE LambdaCase, FlexibleInstances, BlockArguments #-}

module Elaboration.Elaboration
  ( elaboration
  , Elaboration(..)
  )
where

import           Elaboration.Elaborate
import qualified Elaboration.Match as Match
import           Elaboration.ResolveDuality as Dual
import           Elaboration.ResolveEquations
import           Equivalence.Normalisation ( normalise )
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program ( VarEnv, isDatatypeContructor )
import qualified Syntax.Type as T
import           Util.Error
import           Util.FreestState
import           Util.KeepSrc
import           Validation.Kinding (synthetise)
import qualified Validation.Subkind as SK (join)

import           Control.Monad (when)
import           Data.Functor
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Validation.Substitution (free)
import qualified Syntax.Base as T
import           Data.Char (isLower)
import qualified Data.Set as Set

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
  -- debugM . ("VenvI " ++) <$> show . Map.filterWithKey(\k _ -> k == mkVar defaultSpan "rcvInt") =<< getVEnv

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
    fixConsType kEnv m (T.Arrow s _ t u) = do
      (K.Kind _ m' _) <- synthetise kEnv t
      fmap keepSrc $ T.Arrow s (kindToTypeMult m) t <$> fixConsType kEnv (SK.join m m') u
      where kindToTypeMult K.Un = Un
            kindToTypeMult K.Lin = Lin
    fixConsType _ _ t = pure t

-- | Elaboration over environments (VarEnv + ParseEnv)

elabVEnv :: VarEnv -> FreestState ()
elabVEnv = tMapWithKeyM_ (\pv t -> addToVEnv pv . quantifyLowerFreeVars =<< elaborate t)
  where quantifyLowerFreeVars t = 
          foldr (\v t -> keepSrc $ T.Forall p (T.Bind p v (K.ut p) t)) t (Set.filter (isLower.head.show) $ free t)
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
    fmap keepSrc $ E.Abs (getSpan b) m . Bind (getSpan b) b t1 <$> buildExp e bs t2
  buildExp e bs (T.Forall p (Bind p1 x k t)) =
    fmap keepSrc $ E.TypeAbs p . Bind p1 x k <$> buildExp e bs t
  buildExp _ _ t@(T.Dualof _ _) = internalError "Elaboration.Elaboration.buildFunbody.buildExp" t
  buildExp _ xs _ = do
    t <- fromJust <$> getFromVEnv f
    addError (WrongNumberOfArguments (getSpan f) f (length as - length xs) (length as) t) $> e
