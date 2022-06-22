module Utils where

import           Util.FreestState

import           Control.Monad.State
import           System.Console.Haskeline
import           Data.Functor


type REPLState = StateT FreestS IO

liftS :: IO a -> InputT (StateT FreestS IO) a
liftS = lift . lift 
  
wrapExec :: FreestState a ->  REPLState Bool
wrapExec f = do
  s <- gets $ execState f
  if hasErrors s
    then lift (putStrLn $ getErrors s) $> True
    else put s $> False

wrapRun :: Show a => FreestState a ->  REPLState Bool
wrapRun f = do
  (v,s) <- gets $ runState f
  if hasErrors s
    then lift (putStrLn $ getErrors s) $> True
    else lift (print v) >> put s $> False

ignoreFst :: Maybe (a,b) -> Maybe b
ignoreFst (Just (_,t)) = Just t
ignoreFst Nothing = Nothing
