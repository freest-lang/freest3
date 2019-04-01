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

{-# LANGUAGE LambdaCase #-}

module Validation.Typing
( typeCheck
) where

import           Parse.Lexer (position, defaultPos)
import           Syntax.Expression (Expression)
import           Syntax.Types
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import qualified Validation.Kinding as K
import           Validation.TypingExps as T
import           Utils.PreludeLoader (isBuiltin)
import           Control.Monad.State
import           Validation.Extract
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as Trav
import           Debug.Trace

typeCheck :: FreestState ()
typeCheck = do
  -- Type/datatype declarations (TypeEnv)
  tenv <- getTenv
  mapWithKeyM (\b (k,_) -> addToKenv b k) tenv
  tenv <- getTenv
  mapM_ (K.synthetizeTS . snd) tenv
  tenv <- getTenv
  mapWithKeyM (\b _ -> removeFromKenv b) tenv
  -- Function signatures (VarEnv)
  venv <- getVenv
  mapM_ K.synthetizeTS venv
  mapWithKeyM hasBinding venv
  -- -- Function bodies (ExpEnv)
  eenv <- getEenv
  mapWithKeyM checkFunBody eenv
  -- -- Main function
  checkMainFunction

mapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
mapWithKeyM f m = Trav.sequence (Map.mapWithKey f m)

hasBinding :: PBind -> a -> FreestState ()
hasBinding f _ = do
  eenv <- getEenv
  when (not (isBuiltin f) && f `Map.notMember` eenv) $
    addError (position f) ["The type signature for", styleRed $ show f,
                           "lacks an accompanying binding"]

checkFunBody :: PBind -> ([PBind], Expression) -> FreestState ()
checkFunBody f (bs, exp) =
  getFromVenv f >>= \case
    Just t -> do
      let ts = toList t
      params <- buildParams f t bs (init ts)
      mapM_ (uncurry addToVenv) params
      let (TypeScheme _ _ u) = last ts
      T.checkAgainst exp u
      mapM_ (removeFromVenv . fst) params
    Nothing ->
      addError (position f) ["Did not find the signature of function", styleRed $ show f]

buildParams :: PBind -> TypeScheme -> [PBind] -> [TypeScheme] -> FreestState [(PBind, TypeScheme)]
buildParams (PBind p f) (TypeScheme _ _ t) ps ts 
  | binds == params = return $ zip ps ts
  | otherwise = do
      addError p ["The equation for", styleRed f, "has", show binds, "parameter(s)\n",
                  "\t but its type", styleRed (show t), "has", show params]
      return []
  where binds  = length ps
        params = length ts

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
    let (TypeScheme _ _ t') = t
    k <- K.synthetize t'
    when (not b) $
      addError (position mType) ["The type for", styleRed "main", "must be an unrestricted, non-function type\n",
                                 "\t found type", styleRed $ show t, ":", styleRed $ show k]

isValidMainType :: TypeScheme -> FreestState Bool
isValidMainType (TypeScheme _ _ (Fun _ _ _ _)) = return False
isValidMainType t@(TypeScheme _ [] _)          = K.un t
isValidMainType _                              = return False
