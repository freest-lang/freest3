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

typeCheck :: FreestState ()
typeCheck = do
  -- Type/datatype declarations (TypeEnv)
  tenv <- getTenv
  mapWithKeyM (\b (k,_) -> addToKenv b k) tenv
  mapM_ (K.synthetizeTS . snd) tenv
  mapWithKeyM (\b _ -> removeFromKenv b) tenv
  -- Function declarations (VarEnv)
  venv <- getVenv
  mapM_ K.synthetizeTS venv
  -- Function definitions (ExpEnv)
  eenv <- getEenv
  mapWithKeyM checkFunDecl eenv
  -- Main function
  checkMainFunction

mapWithKeyM :: Monad m => (k -> a1 -> m a2) -> Map.Map k a1 -> m (Map.Map k a2)
mapWithKeyM f m = Trav.sequence (Map.mapWithKey f m)

checkFunDecl ::  Bind -> ([Bind], Expression) -> FreestState ()
checkFunDecl f (bs, exp) = do
  venv <- getVenv
  let t = venv Map.! f
      ts = toList t
  params <- buildParams f t bs (init ts)
  mapM_ (\(b, t) -> addToVenv b t) params
  let (TypeScheme _ _ u) = last ts
  T.checkAgainst exp u
  mapM_ (removeFromVenv . fst) params

buildParams :: Bind -> TypeScheme ->  [Bind] -> [TypeScheme] -> FreestState [(Bind, TypeScheme)]
buildParams (Bind p f) (TypeScheme _ _ t) ps ts 
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
  when ((Bind defaultPos "main") `Map.notMember` venv) $
    addError (defaultPos) ["Function", styleRed "main", "is not defined"]  

