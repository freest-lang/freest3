{-# LANGUAGE LambdaCase, FlexibleInstances, TupleSections, NamedFieldPuns #-}
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
import           Syntax.Program ( VarEnv )
import qualified Syntax.Type as T
import           Util.Error
import           Util.FreestState
import           Util.PreludeLoader ( userDefined )

import           Data.Functor
import           Data.List      (transpose)
import           Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

elaboration :: FreestState ()
elaboration = do
  -- | Checks correct number of arguments
  checkNumArgs =<< getPEnvPat
  -- | Checks correct channels' pattern matching
  checkChanVar =<< getPEnvPat
  -- | Adds missing Vars to malformed functions
  (Match.addMissingVars <$> getPEnvPat) >>= setPEnvPat
  -- | Remove all patterns
  (Match.matchFuns =<< getPEnvPat) >>= setPEnv
  -- | Solve the equations' system.
  solveEquations
  -- | From this point, there are no type names on the RHS
  --   of the type declarations and datatypes (type env)
  -- | Substitute all type names on the function signatures
  elabVEnv =<< getVEnv
  -- | same for parse env (which contains the functions' bodies)
  elabPEnv =<< getPEnv
  -- | From this point, there are no type names on the function signatures
  --   and on the function bodies. 
  -- | Then, resolve all the dualof occurrences on:
  -- | Type Env (i.e. type A = dualof !Int)
  (Dual.resolve =<< getTEnv) >>= setTEnv
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

-- | Function validation before translation

checkNumArgs :: ParseEnvPat -> FreestState ()
checkNumArgs pep = tMapWithKeyM_ checkNumArgs' pep

checkNumArgs' :: Variable -> [([E.Pattern],E.Exp)] -> FreestState ()
checkNumArgs' fn lines  
  | allSame $ map (length.fst) lines = return ()
  | otherwise = addError $ DifNumberOfArguments (getSpan fn) fn 
  where allSame (x:y:ys) = x == y && allSame (y:ys)
        allSame _ = True

checkChanVar :: ParseEnvPat -> FreestState ()
checkChanVar penv = do
  cons <- Match.getConstructors -- set with every constructor
  tMapM_ (checkChanVar' cons.transpose.map fst) penv

checkChanVar' :: Set.Set Variable -> [[E.Pattern]] -> FreestState ()
checkChanVar' cons [] = return ()
checkChanVar' cons (xs:xss) = do
  if any Match.isVar xs && any Match.isCon xs then do
    let varsF    = map Match.pVar $ filter Match.isVar xs
    let consF    =                  filter Match.isCon xs
    let consFVar = Set.fromList $ map Match.pVar consF
    let inter    = Set.intersection cons consFVar
    if Set.null inter then
      -- Channel pattern-matching
      mapM (\v -> addError $ InvalidVariablePatternChan (getSpan v) v) varsF
      -- nested patterns
      -- >> checkChanVar' cons [ map Match.pPats $ filter Match.isCons xs ] 
      >> checkChanVar' cons xss
    -- Data types pattern-matching
    else return ()
  else 
    -- No mixture pattern-matching
    return ()

-- | Elaboration over environments (VarEnv + ParseEnv)

elabVEnv :: VarEnv -> FreestState ()
elabVEnv = tMapWithKeyM_ (\pv t -> addToVEnv pv =<< elaborate t) . userDefined

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
