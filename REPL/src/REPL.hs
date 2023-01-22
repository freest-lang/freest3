{-# LANGUAGE NamedFieldPuns #-}
module REPL where

import           Elaboration.Elaboration ( elaboration )
import           HandleOpts
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser
import           Paths_FreeST ( getDataFileName )
import           Syntax.Base
import           Util.FreestState
import           Utils
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )
import qualified Validation.Typing as T

import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Console.Haskeline
import           System.Directory
import           System.Environment
import           System.Exit ( die )
import           System.FilePath

main :: IO ()
main = do
  putStrLn $ replVersion ++ ": https://freest-lang.github.io/  :h for help"
  args <- getArgs
  home <- (</> ".repl_history") <$> getHomeDirectory
  runFilePath <- getDataFileName "Prelude.fst"
  s1 <- parseProgram (initialState {runOpts=defaultOpts{runFilePath}})
  let s2 = emptyPEnv $ execState elaboration s1
  evalStateT (runInputT (replSettings home) (repl s2 args))
    s2{runOpts=defaultOpts{runFilePath="<interactive>"}}

------------------------------------------------------------
-- AUTOCOMPLETE & HISTORY
------------------------------------------------------------

replSettings :: FilePath -> Settings REPLState
replSettings f = Settings
  { complete       = completeFun
  , historyFile    = Just f
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

repl :: FreestS -> [String] -> InputT REPLState ()
repl s [] = handleInterrupt (repl s []) $
    withInterrupt $ getInputLine "λfreest> " >>= parseOpt s >> repl s []
repl s (x:_) = lift (load s x "OK. Module(s) loaded!") >> repl s []
 
-- | OPTIONS

type Option = Maybe String

parseOpt :: FreestS -> Option -> InputT REPLState ()
parseOpt _ Nothing  = liftS $ die "Leaving FreeSTi."
parseOpt s (Just xs)
  | isOpt [":q", ":quit"] = liftS $ die "Leaving FreeSTi."
  | isOpt [":v", ":version"] = liftS $ putStrLn replVersion 
  | isOpt [":h", ":help"] = liftS $ putStrLn helpMenu
  | isOpt [":r", ":reload"] = lift $ reload s
  | opt == ":{" = multilineCmd opt
  | isOpt [":l", ":load"] =  lift $ load s cont "OK. Module(s) loaded!"
  | isOpt [":t",":type"] = lift $ typeOf cont
  | isOpt [":k",":kind"] = lift $ kindOf cont
  | isOpt [":i", ":info"] = lift $ info cont
  | ":" `isPrefixOf` opt = liftS $ putStrLn $ "unknown command '" ++ opt ++ "', use :h for help"
  | isOpt ["data","type"] = do
      st <- lift get
      let s1 = parseDefs st "<interactive>" xs -- opt TODO: CHECK opt xs xs'??
      let s2 = emptyPEnv $ execState (elaboration >> renameState >> typeCheck) s1
      if hasErrors s2
        then liftS $ putStrLn $ getErrors s2
        else lift $ put s2
  | null opt = pure ()
  | otherwise = do
      f <- lift getFileName
      st <- lift get
      case parseExpr f xs of
        Left err -> liftS $ print err
        Right e       -> do
          let s1 = execState (T.synthetise Map.empty e) st
          if hasErrors s1
            then liftS $ putStrLn $ getErrors s1
            else liftS $ evalAndPrint (mkVar defaultSpan "main") st e
  where
    (opt, cont) = splitOption xs
    isOpt = elem opt



