{-|
Module      :  Kinding
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Validation.Kinding
( Kind (..)
, kindOf
, kinding
, isWellKinded
, isSessionType
, contractive
, un
, lin 
, kindOfScheme
, checkAgainstKind
) where

import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Syntax.Kinds
import           Syntax.Terms
import           Syntax.Types
import           Utils.Errors
import           Validation.TypingState

-- Determines whether the type is linear or not
lin :: KindEnv -> Type -> Bool
lin _ (Basic _) = False
lin _ (Fun m _ _) = m == Lin
lin _ (PairType _ _) = False
lin _ (Datatype _) = False
lin _ Skip = False
lin kenv (Semi t1 t2) = lin kenv t1 ||  lin kenv t2 -- WAS: && -- WAS : ||
lin _ (Message _ _) = True
lin _ (Choice _ _) = True
-- TODO: REF Position
lin kenv (Rec x t) = lin (Map.insert (var x) ((0,0), (kind x)) kenv) t
lin kenv (Var x) =
  if Map.member x kenv then
    -- TODO: REF
--    multiplicity (kenv Map.! x) == Lin
    let (_,k) = kenv Map.! x in
    multiplicity k == Lin
  else error $ "Internal error: predicate lin, type var " ++  show x ++ " not in scope" -- should not happen

-- Determines whether the type is unrestricted or not
un :: KindEnv -> Type -> Bool
un kenv t = not (lin kenv t)

-- Is a given type contractive?
contractive :: KindEnv -> Type -> Bool
contractive kenv (Semi t _) = contractive kenv t
contractive kenv (Rec _ t)  = contractive kenv t
contractive kenv (Var x)    = Map.member x kenv
contractive _    _          = True

-- Used when an error is found
topKind :: Kind
topKind = Kind Functional Lin

-- Check variables
checkVar :: Pos -> TypeVar -> TypingState Kind
checkVar p v = do
  b <- kenvMember v
  if b then
    getKind v
  else do
    file <- getFileName
    addError $ styleError file p
               [styleRed $ "'" ++ v ++ "'", "is a free variable."]
    addToKenv p v topKind
    return $ topKind

-- Check if a type is a session type
checkSessionType :: Pos -> Type -> Kind ->  TypingState ()
checkSessionType p t k
  | prekind k == Session = return ()
  | otherwise            = do
      file <- getFileName
      addError $ styleError file p
                 ["Expecting type", styleRed $ show t,
                  "to be a session type; found kind", styleRed $ show k]

-- Checks if all the elements of a choice are session types
checkChoice :: Pos -> [Kind] -> TypingState Kind
checkChoice p ks
   | all (<= Kind Session Lin) ks = return $ Kind Session Lin
   | otherwise  = do
       file <- getFileName
       addError $ styleError file p
                  ["One of the components in a choice isn't lower than SL"]
       return topKind

-- Check the contractivity of a given type; issue an error if not
checkContractivity :: Pos -> KindEnv -> Type -> TypingState ()
checkContractivity p kenv t
  | contractive kenv t = return ()
  | otherwise          = do
      file <- getFileName
      addError $ styleError file p
                 ["Type", styleRed $ show t, "is not contractive"]

-- Determines the kinding of a type

kinding :: Pos -> TypeScheme -> TypingState Kind
kinding p t = synthetizeScheme p t -- ) initialState

synthetizeScheme :: Pos -> TypeScheme -> TypingState Kind
synthetizeScheme p (TypeScheme [] t) = synthetize p t
synthetizeScheme p (TypeScheme bs t) = do
  foldM_ (\_ b -> addToKenv p (var b) (kind b)) () bs
  synthetize p t

synthetize :: Pos -> Type -> TypingState Kind
synthetize p Skip = return $ Kind Session Un
synthetize p (Message _ _) = return $ Kind Session Lin
synthetize p (Basic _)     = return $ Kind Functional Un
synthetize p (Var x)       = checkVar p x
synthetize p (Semi t u) = do
  kt <- synthetize p t 
  ku <- synthetize p u
  checkSessionType p t kt
  checkSessionType p u ku
  return $ Kind Session (max (multiplicity kt) (multiplicity ku))
synthetize p (Fun m t u) = do
  synthetize p t
  synthetize p u
  return $ Kind Functional m
synthetize p (PairType t u) = do
  synthetize p t 
  synthetize p u
  return $ Kind Functional Lin
synthetize p (Datatype m) = do
  ks <- mapM (synthetize p) (Map.elems m)
  return $ Kind Functional (multiplicity $ maximum ks)
synthetize p (Choice _ m) = do
  ks <- mapM (synthetize p) (Map.elems m)
  checkChoice p ks  
synthetize p (Rec (Bind x k) t) = do
  addToKenv p x k
  k1 <- synthetize p t
  kenv <- getKenv
  checkContractivity p kenv t
  removeFromKenv x
  return k1

{- WAS:
checkAgainstKind :: Pos -> Type -> Kind -> TypingState Type
checkAgainstKind p t k = do
  k' <- synthetize p t
  isSubKindOf k' k
  return t
-}

checkAgainstKind :: Pos -> Type -> Kind -> TypingState ()
checkAgainstKind p t k = do
  k' <- synthetize p t
  isSubKindOf p k' k
  return ()

isSubKindOf :: Pos -> Kind -> Kind -> TypingState ()
isSubKindOf p k1 k2
  | k1 <= k2  = return ()
  | otherwise = do
      file <- getFileName
      addError $ styleError file p
                  ["Expection kind", styleRed $ show k1, "to be a sub-kind of kind",
                   styleRed $ show k2, "but it isn't."]
      

-- TODO: review & union should not be necessary
-- TODO: filename
kindOf :: Pos -> KindEnv -> Type -> Kind
kindOf p k t =
  let (f, venv, eenv, cenv, kenv, err) = (initialState  "filename") in
  evalState (synthetize p t) (f, venv, eenv, cenv, Map.union k kenv, err)

kindOfScheme :: Pos -> TypeScheme -> Kind
kindOfScheme p t = evalState (synthetizeScheme p t) (initialState "filename")


isWellKinded :: Pos -> KindEnv -> Type -> Bool
isWellKinded p k t =
  let (f, venv, eenv, cenv, kenv, err) = (initialState "filename") in
  let (_,_,_,_,_,errors) =
        execState (synthetize p t) (f, venv, eenv, cenv, Map.union k kenv, err) in
    null errors

isSessionType' :: Pos -> KindEnv -> Type -> Bool
isSessionType' p kenv t = isWellKinded p kenv t && prekind (kindOf p kenv t) == Session

-- TODO: TMP: while typeEquivalence is not changed
isSessionType :: KindEnv -> Type -> Bool
isSessionType kenv t = isWellKinded (0,0) kenv t && prekind (kindOf (0,0) kenv t) == Session

-- Need this ?? 
--kindErr :: Type -> [String]
--kindErr t =
--  let (_, _, err) = execState (synthetize t) initialState in err


-- runState :: State s a -> s -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s
