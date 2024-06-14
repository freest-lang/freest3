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

import Syntax.Expression
import Interpreter.Value (Ctx)
import Parse.Phase (ParseS)
import Syntax.AST

data Repl

type instance XDef Repl = Exp

data Extra = Extra 
  { runOpts :: S.RunOpts 
  , ctx     :: Ctx
  }

type instance S.XExtra Repl = Extra

type ReplS = S.FreestS Repl

type ReplState = StateT ReplS IO

interactiveRunOpts :: S.RunOpts
interactiveRunOpts = S.defaultOpts{S.runFilePath="<interactive>"}


setFilePath :: FilePath -> ReplState ()
setFilePath runFilePath =
  modify (\s@S.FreestS{S.extra=extra} -> s{S.extra = extra{runOpts=(runOpts extra){S.runFilePath}}})

getFilePath :: ReplState FilePath
getFilePath = gets $ S.runFilePath . runOpts . S.extra
--  modify (\s -> s {S.extra = (S.extra s){S.runFilePath}})

getRunOpts :: ReplState S.RunOpts
getRunOpts = gets (runOpts . S.extra)


pretty :: Show a => String -> a -> ReplState ()
pretty prefix x = do
  s <- get
  if S.hasErrors s
  then replErrors
  else liftIO $ putStrLn (prefix ++ " : " ++ show x)
       

replErrors :: ReplState ()
replErrors = do
  s <- get
  S.setErrors []
  setFilePath "<interactive>"
  runOpts <- gets (runOpts . S.extra)
  liftIO $ putStrLn (S.getErrors runOpts s)




-- liftS :: IO a -> InputT (StateT ReplS IO) a
liftS :: IO a -> InputT ReplState a
liftS = lift . lift 

-- wrapExec :: S.FreestState a b ->  ReplState Bool
-- wrapExec :: S.State (S.FreestS a) b -> ReplState Bool
-- wrapExec f = do
--   s <- gets $ execState f
--   if S.hasErrors s
--     then lift (putStrLn $ S.getErrors runOpts s) $> True
--     else put s $> False

-- wrapExec :: FreestState a b ->  ReplState Bool
-- wrapExec f = do
--   s <- gets $ execState f
--   if hasErrors s
--     then lift (putStrLn $ getErrors s) $> True
--     else put s $> False

-- wrapExec_ :: FreestState a ->  ReplState ()
-- wrapExec_  = void . wrapExec

-- wrapIO :: IO (S.FreestS a) -> ReplState () ->  ReplState Bool
-- wrapIO io cont = do
--   s <- lift io
--   if S.hasErrors s
--     then lift (putStrLn $ S.getErrors interactiveRunOpts s) $> True
--     else put s >> cont $> False

-- wrapIO_ :: IO FreestS -> ReplState () -> ReplState ()
-- wrapIO_ io state = void $ wrapIO io state

-- wrapRun :: Show a => FreestState a ->  ReplState Bool
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
    

typingToRepl :: Ctx -> TypingS -> ReplS   
typingToRepl ctx s = 
  s{ S.ast   = (S.ast s){definitions=definitions (S.ast s)}
   , S.extra = Extra{runOpts=S.extra s, ctx}
   }

replToTyping :: ReplS -> (TypingS, Ctx)   
replToTyping s = 
  ( s{ S.ast   = (S.ast s){definitions=definitions (S.ast s)}
     , S.extra = runOpts(S.extra s)
     }
  , ctx (S.extra s)
  )
