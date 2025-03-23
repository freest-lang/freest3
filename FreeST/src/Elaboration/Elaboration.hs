{-# LANGUAGE LambdaCase #-}

module Elaboration.Elaboration (elaboration) where

import qualified Syntax.Base as T
import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Type as T
import qualified Elaboration.Phase as EP
import           Elaboration.Replace
import           Elaboration.ResolveDuality as Dual
import           Elaboration.ResolveEquations
import           Elaboration.Phase
import           Typing.Normalisation ( normalise )
import qualified Typing.Phase as VP
import qualified PatternMatch.Phase as PMP
import           Util.Error
import           Util.State

import           Control.Monad
import qualified Control.Monad.State as S
import           Data.Char (isLower)
import           Data.Functor hiding (void)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set


elaboration :: Set.Set Variable -> Set.Set Variable -> PMP.PatternS -> (VP.Defs, ElabS)
elaboration pkvs mvs patternS = S.runState elaboration' (patternToElab patternS)
  where
    patternToElab :: PMP.PatternS -> ElabS
    patternToElab s = s {ast = (ast s){definitions = definitions (ast s)}
                        , extra = EP.Extra{EP.mVariables=mvs, EP.pkVariables=pkvs} }
    
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
  getSignatures >>= quantifyPoly
  solveEquations
  getTypes >>= Dual.resolve >>= setTypes
  getSignatures >>= replaceSignatures
  getDefs >>= replaceDefinitions
  getSignatures >>= Dual.resolve >>= setSignatures
  getDefs >>= Dual.resolve >>= setDefs
  getDefs >>= buildDefs  
  -- debugM . ("Program " ++) <$> show . Map.filterWithKey(\k _ -> k == mkVar defaultSpan "rcvInt") =<< getProg
  -- debugM . ("VenvI " ++) <$> show . Map.filterWithKey(\k _ -> k == mkVar defaultSpan "rcvInt") =<< getVEnv
  -- return defs

-- | Elaboration over environments (Signatures & Definitions)

replaceSignatures :: Signatures -> ElabState ()
replaceSignatures = tMapWithKeyM_ (\pv t -> addToSignatures pv =<< replace t)

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
  buildExp e (b : bs) (T.Arrow _ m l1 l2 t1 t2) =
    E.Abs (getSpan b) m . Bind (getSpan b) b t1 <$> buildExp e bs t2 --levels not in use here, could be wrong
  buildExp e bs (T.Forall p (Bind p1 x k t)) =
    E.TypeAbs p . Bind p1 x k <$> buildExp e bs t
  buildExp _ _ t@(T.Dualof _ _) = internalError "Elaboration.Elaboration.buildFunbody.buildExp" t
  buildExp _ xs _ = do
    t <- fromJust <$> getFromSignatures f
    addError (WrongNumberOfArguments (getSpan f) f (length as - length xs) (length as) t) $> e


-- | Quantify polymorphic variables
quantifyPoly :: Signatures -> ElabState ()
quantifyPoly = tMapWithKeyM_ (\v t -> quantifyLowerFreeVars t >>= addToSignatures v)
  where
    quantifyLowerFreeVars t = 
      foldM (\t v -> freshKVar p >>= \k -> pure $ T.Forall p (T.Bind p v k t)) t
                (reverse $ Set.toList $ Set.filter (isLower . head . show) $ T.free t)
      where p = getSpan t
