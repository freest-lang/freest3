{-|
Module      :  Typing
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

{-# LANGUAGE LambdaCase, NoMonadFailDesugaring #-}

module Validation.Typing
( typeCheck
) where

import           Parse.Lexer (position, defaultPos)
import           Syntax.Expression (Expression(..))
import           Syntax.Types
import           Syntax.Bind
import           Syntax.Programs
import           Utils.Errors
import           Utils.FreestState
import qualified Validation.Kinding as K
import           Validation.TypingExps as T
import           Control.Monad.State
import           Validation.Extract
import           Utils.PreludeLoader (isBuiltin)
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as Trav
import           Debug.Trace

mapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
mapWithKeyM f m = Trav.sequence (Map.mapWithKey f m)

typeCheck :: FreestState ()
typeCheck = do
  -- Type/datatype declarations (TypeEnv)
  tenv <- getTenv
  mapWithKeyM (\b (k,_) -> addToKenv b k) tenv
  tenv <- getTenv
  mapM_ (K.synthetiseTS . snd) tenv
  tenv <- getTenv
  mapWithKeyM (\b _ -> removeFromKenv b) tenv -- TODO: only works if all vars in the prog are distinct
  -- Function signatures (VarEnv)
  venv <- getVenv
  mapM_ K.synthetiseTS venv
  mapWithKeyM hasBinding venv
  -- Function bodies (ExpEnv)
  eenv <- getEenv
  mapWithKeyM checkFunBody eenv
  -- Main function
  checkMainFunction

-- Check whether all functions signatures have a binding. Exclude the
-- builtin functions and the datatype constructors.
hasBinding :: PBind -> a -> FreestState ()
hasBinding f _ = do
  eenv <- getEenv
  venv <- getVenv
  when (not (isDatatypeContructor venv f) && not (isBuiltin f) && f `Map.notMember` eenv) $
    addError (position f) ["The type signature for", styleRed $ show f,
                           "lacks an accompanying binding"]

-- To determine whether a given constructor (a program variable) is a
-- datatype constructor we have to look in the type environment for a
-- type name associated to a datatype that defines the constructor
-- (rather indirect)
isDatatypeContructor :: VarEnv -> PBind -> Bool
isDatatypeContructor venv c = do
  Map.foldr (\(TypeScheme _ _ t) acc -> acc || isDatatype t) False venv
  where isDatatype :: Type -> Bool
        isDatatype (Datatype _ m) = c `Map.member` m
        isDatatype _              = False

-- Check whether there is a signature for a given function. Take the
-- chance to replace the dummy Unit types in the bodies of functions.
checkFunBody :: PBind -> Expression -> FreestState ()
checkFunBody f e =
  getFromVenv f >>= \case
    Just ts -> do
      e' <- fillFunType f e ts
      T.checkAgainstST e' ts
    Nothing ->
      addError (position f) ["Did not find the signature of function", styleRed $ show f]

-- At parsing time all lambda variables in function definitions are
-- associated to type Unit. Here we amend the situation by replacing
-- these types with those declared in the type scheme for the
-- function.
fillFunType :: PBind -> Expression -> TypeScheme -> FreestState Expression
fillFunType b@(PBind p f) e (TypeScheme _ _ t) = do
  e' <- fill e t
  addToEenv b e'
  return e'
  where
  fill :: Expression -> Type -> FreestState Expression
  fill (Lambda p _ x _ e) (Fun _ m t1 t2) = do
    e' <- fill e t2
    return $ Lambda p m x t1 e'
  fill e@(Lambda p _ _ _ _) t = do
    addError p ["Couldn't match expected type", styleRed $ show t, "\n",
                "\t The equation for", styleRed f, "has one or more arguments,\t",
                "\t but its type", show t, "has none"]
    return e
  fill e _ = return e

checkMainFunction :: FreestState ()
checkMainFunction = do
  venv <- getVenv
  let mBind = PBind defaultPos "main"
  if mBind `Map.notMember` venv
  then
    addError defaultPos [styleRed "main", "is not defined"]
  else do
    let t = venv Map.! mBind
    mType <- normaliseTS t
    b <- isValidMainType mType
    k <- K.synthetiseTS t
    when (not b) $
      addError (position mType) ["The type for", styleRed "main", "must be an unrestricted, non-function type\n",
                                 "\t found type", styleRed $ show t, ":", styleRed $ show k]

isValidMainType :: TypeScheme -> FreestState Bool
isValidMainType (TypeScheme _ _ (Fun _ _ _ _)) = return False
isValidMainType t@(TypeScheme _ [] _)          = K.un t
isValidMainType _                              = return False
