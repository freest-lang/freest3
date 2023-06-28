{-# LANGUAGE LambdaCase, FlexibleInstances, BlockArguments , TypeFamilies #-}

module Elaboration.Elaboration
  ( elaboration
  -- , Elaboration(..)
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
-- import           Util.FreestState
import           Validation.Kinding (synthetise)
import qualified Validation.Subkind as SK (join)
import           Parse.Phase
import           Elaboration.Phase
import           Validation.Phase  -- (Prog, Typing)
import           Util.State.State
import           Syntax.AST

import           Control.Monad (when)
import           Control.Monad.State
import           Data.Functor
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Validation.Substitution (free)
import qualified Syntax.Base as T
import           Data.Char (isLower)
import qualified Data.Set as Set
import Debug.Trace 

-- | Elaborate main functions
-- | 1. Resolves pattern matching (removes patterns)
-- | 2. Resolves typenames, duality & builds the program
elaboration :: ParseEnvPat -> FreestPattern -> ElabState Prog
elaboration s initS = do
  let s1 = execState (patternMatching s) initS
  put (convertState s1)
  elab
  where
    convertState :: FreestPattern -> FreestElab
    convertState s = s {ast = (ast s){definitions = definitions (ast s)}
                       , extra = extra s}

-- | 1. Fix the multiplicity of the data constructor types
-- | 2. Checks if there are choices with the same name as constructors (no need for now)
-- | 3. Checks correct number of arguments
-- | 4. Checks correct channels' pattern matching
-- | 5. Adds missing Vars to malformed functions
-- | 6. Remove all patterns
patternMatching :: ParseEnvPat -> PatternState ()
patternMatching s = do
  fixConsTypes
  --  Match.checkChoices =<< getPEnvChoices
  Match.checkNumArgs s
  Match.checkChanVar s
  let s1 = Match.addMissingVars s
  Match.matchFuns s1 >>= setDefs

-- | 1. Solve the equations' system.
-- |    From this point, there are no type names on the function signatures
--      and on the function bodies. 
-- | 2. Resolve all the dualof occurrences on types (i.e. type A = dualof !Int)
--      From this point, there are no type names on the RHS
--      of the type declarations and datatypes (type env)
-- | 3. Substitute all type names on the function signatures
-- | 4. Same for parse env (which contains the functions' bodies)
-- | 5. Resolve all the dualof occurrences on signatures (i.e. f : dualof !Int -> Skip)
-- | 6. Resolve all the dualof occurrences on definitions (i.e. f c = send 5 c)
-- |    From this point there are no more occurrences of the dualof operator
-- | 7. Build the expression environment: substitute all
--      type operators on ExpEnv;
--      From f x = E and f : T -> U
--      build a lambda expression: f = \x : T -> E
elab :: ElabState Prog
elab = do
  solveEquations
  (Dual.resolve =<< getTypes) >>= setTypes
  elabVEnv =<< getSignatures
  elabPEnv =<< getDefs
  (Dual.resolve =<< getSignatures) >>= setSignatures
  (Dual.resolve =<< getDefs) >>= setDefs
  buildProg =<< getDefs
  -- debugM . ("Program " ++) <$> show =<< getProg
  -- debugM . ("VenvI " ++) <$> show . Map.filterWithKey(\k _ -> k == mkVar defaultSpan "rcvInt") =<< getVEnv

-- | Fix the multiplicity of the data constructor types
fixConsTypes :: PatternState ()
fixConsTypes = do
  tEnv <- getTypes
  -- if this is the first step in the elaboration, there are still type names in signatures,
  -- so we need a non-empty kind environment. Empty env otherwise.
  let kEnv = Map.map fst tEnv
  getSignatures >>= tMapWithKeyM_ \k v -> when (isDatatypeContructor k tEnv)
    (fixConsType kEnv K.Un v >>= addToSignatures k)
  where
    fixConsType :: K.KindEnv -> K.Multiplicity -> T.Type -> PatternState T.Type
    fixConsType kEnv m (T.Arrow s _ t u) = do
      (K.Kind _ m' _) <- synthetise kEnv t
      T.Arrow s (kindToTypeMult m) t <$> fixConsType kEnv (SK.join m m') u
      where kindToTypeMult K.Un = Un
            kindToTypeMult K.Lin = Lin
    fixConsType _ _ t = pure t

-- | Elaboration over environments (VarEnv + ParseEnv)

elabVEnv :: Signatures -> ElabState ()
elabVEnv = tMapWithKeyM_ (\pv t -> addToSignatures pv . quantifyLowerFreeVars =<< elaborate t)
  where quantifyLowerFreeVars t = 
          foldr (\v t -> T.Forall p (T.Bind p v (K.ut p) t)) t (Set.filter (isLower.head.show) $ free t)
          where p = getSpan t

elabPEnv :: ParseEnv -> ElabState ()
elabPEnv = tMapWithKeyM_ (\x (ps, e) -> curry (addToDefinitions x) ps =<< elaborate e)

-- | Build a program from the parse env

buildProg :: ParseEnv -> ElabState (Map.Map Variable E.Exp)
buildProg = Map.foldlWithKey (\prog pv (ps,e) -> addToProg prog pv =<< buildFunBody pv ps e)
             (return Map.empty)
  where addToProg acc pv e = acc >>= \prog -> return $ Map.insert pv e prog

buildFunBody :: Variable -> [Variable] -> E.Exp -> ElabState E.Exp
buildFunBody f as e = getFromSignatures f >>= \case
    Just s  -> buildExp e as s
    Nothing -> addError (FuctionLacksSignature (getSpan f) f) $> e
 where
  buildExp :: E.Exp -> [Variable] -> T.Type -> ElabState E.Exp
  buildExp e [] _ = pure e
  buildExp e bs t@(T.Rec _ _) = buildExp e bs (normalise t)
  buildExp e (b : bs) (T.Arrow _ m t1 t2) =
    E.Abs (getSpan b) m . Bind (getSpan b) b t1 <$> buildExp e bs t2
  buildExp e bs (T.Forall p (Bind p1 x k t)) =
    E.TypeAbs p . Bind p1 x k <$> buildExp e bs t
  buildExp _ _ t@(T.Dualof _ _) = internalError "Elaboration.Elaboration.buildFunbody.buildExp" t
  buildExp _ xs _ = do
    t <- fromJust <$> getFromSignatures f
    addError (WrongNumberOfArguments (getSpan f) f (length as - length xs) (length as) t) $> e

