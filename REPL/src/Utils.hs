{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Utils where

import qualified Util.State as S
import           Typing.Phase

import           Control.Arrow ((***))
import           Control.Monad.State
import           Data.Char (isSpace)
import           Data.List
import           System.Console.Haskeline

-- import Syntax.Expression

-- data REPL

-- type instance XDef REPL = Exp
-- type instance S.XExtra REPL = S.RunOpts

-- type ReplS = S.FreestS REPL
type REPLState = StateT TypingS IO

interactiveRunOpts :: S.RunOpts
interactiveRunOpts = S.defaultOpts{S.runFilePath="<interactive>"}


setFilePath :: FilePath -> REPLState ()
setFilePath runFilePath =
  modify (\s -> s {S.extra = (S.extra s){S.runFilePath}})

getFilePath :: REPLState FilePath
getFilePath = gets $ S.runFilePath . S.extra
--  modify (\s -> s {S.extra = (S.extra s){S.runFilePath}})

getRunOpts :: REPLState S.RunOpts
getRunOpts = gets S.extra


pretty :: Show a => String -> a -> REPLState ()
pretty prefix x = do
  s <- get
  if S.hasErrors s
  then replErrors
  else liftIO $ putStrLn (prefix ++ " : " ++ show x)
       

replErrors :: REPLState ()
replErrors = do
  s <- get
  S.setErrors []
  setFilePath "<interactive>"
  runOpts <- gets S.extra
  liftIO $ putStrLn (S.getErrors runOpts s)




-- liftS :: IO a -> InputT (StateT REPLS IO) a
liftS :: IO a -> InputT REPLState a
liftS = lift . lift 

-- wrapExec :: S.FreestState a b ->  REPLState Bool
-- wrapExec :: S.State (S.FreestS a) b -> REPLState Bool
-- wrapExec f = do
--   s <- gets $ execState f
--   if S.hasErrors s
--     then lift (putStrLn $ S.getErrors runOpts s) $> True
--     else put s $> False

-- wrapExec :: FreestState a b ->  REPLState Bool
-- wrapExec f = do
--   s <- gets $ execState f
--   if hasErrors s
--     then lift (putStrLn $ getErrors s) $> True
--     else put s $> False

-- wrapExec_ :: FreestState a ->  REPLState ()
-- wrapExec_  = void . wrapExec

-- wrapIO :: IO (S.FreestS a) -> REPLState () ->  REPLState Bool
-- wrapIO io cont = do
--   s <- lift io
--   if S.hasErrors s
--     then lift (putStrLn $ S.getErrors interactiveRunOpts s) $> True
--     else put s >> cont $> False

-- wrapIO_ :: IO FreestS -> REPLState () -> REPLState ()
-- wrapIO_ io state = void $ wrapIO io state

-- wrapRun :: Show a => FreestState a ->  REPLState Bool
-- wrapRun f = do
--   (v,s) <- gets $ runState f
--   if hasErrors s
--     then lift (putStrLn $ getErrors s) $> True
--     else lift (print v) >> put s $> False

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

-- -- | ------------------------------------------------------------
-- -- | Stops FreeST pipeline
-- -- | If it finds an error, no longer proceeds to the continuation 
-- -- | Usage example: action1 >> stopPipeline (action2 >> action3)
-- -- | ------------------------------------------------------------

-- stopPipeline :: FreestState () -> FreestState ()
-- stopPipeline = ifM (gets hasErrors) (return ())
    

