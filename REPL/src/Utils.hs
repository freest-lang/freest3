module Utils where

import Util.FreestState

import Control.Arrow ((***))
import Control.Monad (join)
import Control.Monad.State
import Data.Char (isSpace)
import Data.Functor
import Data.List
import System.Console.Haskeline


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

-- | Handle command line options
-- Removes spaces and split option

-- dropOption :: String -> String
-- dropOption = dropBothEnds . takeWhile (not . isSpace) . dropWhile isSpace

dropBothEnds :: String -> String
dropBothEnds = dropWhileEnd isSpace . dropWhile isSpace

splitOption :: String -> (String, String)
splitOption = join (***) dropBothEnds . break isSpace
