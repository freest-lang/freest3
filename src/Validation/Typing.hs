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
  trace ("Var Env:  " ++ show venv) (return ())
  trace ("Type Env: " ++ show tenv) (return ())
  mapM_ K.synthetiseTS venv
  mapWithKeyM hasBinding venv
  -- -- Function bodies (ExpEnv)
  eenv <- getEenv
  trace ("checkFunBodies before: " ++ show eenv ++ "\n\n") (return ())
  mapWithKeyM checkFunBody eenv
  eenv <- getEenv
  trace ("Fun bodies after: " ++ show eenv) (return ())
  -- Main function
  checkMainFunction

hasBinding :: PBind -> a -> FreestState ()
hasBinding f _ = do
  eenv <- getEenv
  b <- isDatatypeContructor f
  when (not b && not (isBuiltin f) && f `Map.notMember` eenv) $
    addError (position f) ["The type signature for", styleRed $ show f,
                           "lacks an accompanying binding"]

isDatatypeContructor :: PBind -> FreestState Bool
isDatatypeContructor c = do
  tenv <- getTenv
  return $ Map.foldl (\acc (_, (TypeScheme _ _ t)) -> acc || isDatatype t) False tenv
  where isDatatype (Datatype _ m) = c `Map.member` m
        isDatatype _              = False

checkFunBody :: PBind -> Expression -> FreestState ()
checkFunBody f e =
  getFromVenv f >>= \case
    Just ts -> do
      trace ("checkFunBody: checkAgainstST before: " ++ show e ++ "\n") (return ())
      e' <- fillFunType f e ts
      addToEenv f e'
      trace ("checkFunBody: checkAgainstST after: " ++ show e ++ "\n") (return ())
      tenv <- getTenv
      T.checkAgainstST e ts
    Nothing ->
      addError (position f) ["Did not find the signature of function", styleRed $ show f]

fillFunType :: PBind -> Expression -> TypeScheme -> FreestState Expression
fillFunType (PBind p f) e (TypeScheme _ _ t) = fill e t
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

{-
-- TODO: this is a complete hack.
checkFunBody :: PBind -> Expression -> FreestState ()
checkFunBody f (bs, exp) =
  getFromVenv f >>= \case
    Just t -> do
      let (TypeScheme _ bs' _) = t
      mapM_ (\(TBindK p x k) -> addToKenv (TBind p x) k) bs'
      let ts = toList t
      params <- buildParams f t bs (init ts)
      mapM_ (uncurry addToVenv) params
      T.checkAgainstST exp (last ts)
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
-}
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
