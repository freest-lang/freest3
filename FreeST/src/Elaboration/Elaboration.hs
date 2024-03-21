{-# LANGUAGE LambdaCase, FlexibleInstances, BlockArguments , TypeFamilies #-}

module Elaboration.Elaboration (elaboration) where

-- import qualified Elaboration.Match as Match
import           Elaboration.Replace
import           Elaboration.ResolveDuality as Dual
import           Elaboration.ResolveEquations
import           Validation.Normalisation ( normalise )
import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program ( isDatatypeContructor )
import qualified Syntax.Type as T
import           Util.Error
import           Util.State
import           Validation.Kinding (synthetise)
import qualified Validation.Subkind as SK (join)
import qualified PatternMatch.Phase as PMP
import           Elaboration.Phase
import qualified Validation.Phase as VP

import           Control.Monad.State hiding (void)
import           Data.Char (isLower)
import           Data.Functor hiding (void)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Syntax.Base as T
import           Validation.Substitution (free)

elaboration :: PMP.PatternS -> (VP.Defs, ElabS)
elaboration patternS = runState elaboration' (patternToElab patternS)

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
elaboration' :: ElabState VP.Defs
elaboration'  = do
  fixConsTypes
  solveEquations
  getTypes >>= Dual.resolve >>= setTypes
  getSignatures >>= replaceSignatures
  getDefs >>= replaceDefinitions
  getSignatures >>= Dual.resolve >>= setSignatures
  getDefs >>= Dual.resolve >>= setDefs
  getDefs >>= buildDefs
  -- debugM . ("Program " ++) <$> show =<< getProg
  -- debugM . ("VenvI " ++) <$> show . Map.filterWithKey(\k _ -> k == mkVar defaultSpan "rcvInt") =<< getVEnv


patternToElab :: PMP.PatternS -> ElabS
patternToElab s = s {ast = (ast s){definitions = definitions (ast s)}, extra = void}

-- | Fix the multiplicity of the data constructor types
fixConsTypes :: ElabState ()
fixConsTypes = do
  tys <- getTypes
  -- if this is the first step in the elaboration, there are still type names in signatures,
  -- so we need a non-empty kind environment. Empty env otherwise.
  let kEnv = Map.map fst tys
  getSignatures >>= tMapWithKeyM_ \k v -> when (isDatatypeContructor k tys)
    (fixConsType kEnv K.Un v >>= addToSignatures k)
  where
    fixConsType :: K.KindEnv -> K.Multiplicity -> T.Type -> ElabState T.Type
    fixConsType kEnv m (T.Arrow s _ t u) = do
      (K.Kind _ m' _) <- synthetise kEnv t
      T.Arrow s (kindToTypeMult m) t <$> fixConsType kEnv (SK.join m m') u
      where kindToTypeMult K.Un = Un
            kindToTypeMult K.Lin = Lin
    fixConsType _ _ t = pure t

-- | Elaboration over environments (Signatures & Definitions)

replaceSignatures :: Signatures -> ElabState ()
replaceSignatures = tMapWithKeyM_ (\pv t -> addToSignatures pv . quantifyLowerFreeVars =<< replace t)
  where quantifyLowerFreeVars t = 
          foldr (\v t -> T.Forall p (T.Bind p v (K.ut p) t))
                t
                (Set.filter (isLower.head.show) $ free t)
          where p = getSpan t

replaceDefinitions :: Defs -> ElabState ()
replaceDefinitions = tMapWithKeyM_ (\x (ps, e) -> curry (addToDefinitions x) ps =<< replace e)

-- | Build a program from the parse env

buildDefs :: Defs -> ElabState VP.Defs 
buildDefs = Map.foldlWithKey (\def pv (ps,e) -> addToDefs def pv =<< buildFunBody pv ps e)
             (return Map.empty)
  where addToDefs acc pv e = acc >>= \def -> return $ Map.insert pv e def

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


