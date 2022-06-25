{-# LANGUAGE NamedFieldPuns #-}
module REPL where

import           Elaboration.Elaboration ( elaboration )
import           HandleOpts
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser
import           Util.FreestState
import           Utils
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )
import qualified Validation.Typing as T

import           Control.Monad.State
import           Data.Char (isSpace)
import           Data.List
import qualified Data.Map.Strict as Map
import           Paths_FreeST ( getDataFileName )
import           System.Console.Haskeline
import           System.Exit ( die )



main :: IO ()
main = do
  runFilePath <- getDataFileName "Prelude.fst"
  s1 <- parseProgram (initialState {runOpts=defaultOpts{runFilePath}})
  evalStateT (runInputT replSettings repl)
    s1{runOpts=defaultOpts{runFilePath="<interactive>"}}


------------------------------------------------------------
-- AUTOCOMPLETE & HISTORY
------------------------------------------------------------

replSettings :: Settings REPLState
replSettings = Settings
  { complete       = completeFun
  , historyFile    = Just ".repl_history"
  , autoAddHistory = True
  }

completeFun ::CompletionFunc REPLState
completeFun = completeWordWithPrev Nothing " " completionGenerator

completionGenerator :: String -> String -> REPLState [Completion]
completionGenerator "" "" = return []
completionGenerator "" opts = mkCompletion $ filter (opts `isPrefixOf`) optionList
completionGenerator " l:" suffix = lift $ listFiles suffix
completionGenerator " i:" suffix = do
  venv <- getVEnv
  tenv <- getTEnv
  mkCompletion $ filter (suffix `isPrefixOf`) (convert venv ++ convert tenv)
  where
    convert :: Show a => Map.Map a b -> [String]
    convert = map show . Map.keys
completionGenerator _ suffix = do
  venv <- getVEnv
  mkCompletion $ filter (suffix `isPrefixOf`) (map show $ Map.keys venv)

mkCompletion :: Applicative f => [String] -> f [Completion]
mkCompletion = pure . map (\s -> Completion s s False)


optionList :: [String]
optionList =
  [":help",":h"
  ,":quit",":q"
  ,":load",":l"
  ,":type",":t"
  ,":kind",":k"
  ,":info",":i"
  ,":reload",":r"
  , ":{",":}"]


-- | Runs the REPL 

repl :: InputT REPLState ()
repl = getInputLine "λfreest> " >>= parseOpt >> repl

-- | OPTIONS

type Option = Maybe String

parseOpt :: Option -> InputT REPLState ()
parseOpt Nothing  = return ()
parseOpt (Just xs)
  | xs' == ":q" || xs' == ":quit" = liftS $ die "Leaving FreeSTi."
  | xs' == ":h" || xs' == ":help" = liftS $ putStrLn helpMenu
  | xs' == ":r" || xs' == ":reload" = lift reload
  | xs' == ":{" = multilineCmd xs'
  | isOpt [":l ", ":load "] = lift $ load (opt xs')
  | isOpt [":t ",":type "] = lift $ typeOf (opt xs')
  | isOpt [":k ",":kind "] = lift $ kindOf (opt xs')
  | isOpt [":i ", ":info "] = lift $ info (opt xs')     
  | ":" `isPrefixOf` xs' = liftS $ putStrLn $ "unknown command '" ++ xs' ++ "', use :h for help"
  | isOpt ["data ","type "]= do
      st <- lift get
      let s1 = parseDefs st "<interactive>" xs'
      let s2 = emptyPEnv $ execState (elaboration >> renameState >> typeCheck) s1
      if hasErrors s2
        then liftS $ putStrLn $ getErrors s2
        else lift $ put s2
  | null (dropWhile isSpace xs') = pure ()
  | otherwise = do
      f <- lift getFileName
      st <- lift get
      case parseExpr f xs' of
        Left err -> liftS (print err) 
        Right e       -> do
          let s1 = execState (T.synthetise Map.empty e) st
          if hasErrors s1
            then liftS $ putStrLn $ getErrors s1
            else liftS $ evalAndPrint st e
  where
    isOpt :: [String] -> Bool
    isOpt = any (`isPrefixOf` xs)

    opt :: String -> String
    opt = drop 1 . dropWhile (not . isSpace)

    xs' = dropWhileEnd isSpace $ dropWhile isSpace xs
