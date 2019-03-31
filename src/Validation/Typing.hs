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

import           Parse.Lexer (defaultPos)
import           Syntax.Expression (Expression)
import           Syntax.Types
import           Syntax.Bind
import           Utils.Errors
import           Utils.FreestState
import qualified Validation.Kinding as K
import           Validation.TypingExps as T
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as Trav
import qualified Data.Foldable as F

typeCheck :: FreestState ()
typeCheck = do
  -- Type/datatype declarations (TypeEnv)
  tenv <- getTenv
  mapWithKeyM (\b (k,_) -> addToKenv b k) tenv
  F.mapM_ (checkTypeScheme . snd) tenv
  mapWithKeyM (\b _ -> removeFromKenv b) tenv
  -- Function declarations (VarEnv)
  venv <- getVenv
  F.mapM_ checkTypeScheme venv
  -- Function definitions (ExpEnv)
  eenv <- getEenv
  mapWithKeyM checkFunDecl eenv
  -- Main function
  checkMainFunction

mapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
mapWithKeyM f m = Trav.sequence (Map.mapWithKey f m)

checkTypeScheme :: TypeScheme -> FreestState ()
checkTypeScheme (TypeScheme _ ks t) = do
  resetKEnv
  mapM_ (\(KBind p x k) -> addToKenv (Bind p x) k) ks
  K.synthetize t
  return ()  

checkFunDecl ::  Bind -> ([Bind], Expression) -> FreestState ()
checkFunDecl f (bs, exp) = do
  venv <- getVenv
  let t = venv Map.! f
      ts = toList t
  params <- buildParams f bs (init ts)
  mapM_ (\(b, t) -> addToVenv b t) params
  let (TypeScheme _ _ u) = last ts
  T.checkAgainst exp u
  mapM_ (removeFromVenv .fst) params

buildParams :: Bind -> [Bind] -> [TypeScheme] -> FreestState [(Bind, TypeScheme)]
buildParams (Bind p f) ps ts 
  | length ps == length ts = return $ zip ps ts
  | length ps > length ts = do
      addError p ["Function", styleRed f, "has too many parameters"]
      return []
  | otherwise = do
      addError p ["Function ", styleRed f, "has too few parameter"]
      return []

checkMainFunction :: FreestState ()
checkMainFunction = do
  venv <- getVenv
  when ((Bind defaultPos "main") `Map.notMember` venv) $
    addError (defaultPos) ["Function", styleRed "main", "is not defined"]    

