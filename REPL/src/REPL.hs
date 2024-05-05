module REPL where

import           Elaboration.Elaboration ( elaboration )
import           FreeST hiding (main)
import           HandleOpts
import           Inference.Inference
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser
import           Parse.Phase
import           Paths_FreeST ( getDataFileName )
import           PatternMatch.PatternMatch
import           Syntax.Base
import           Typing.Rename ( renameProgram )
import qualified Typing.Typing as T
import           Util.State
import           Utils

import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Console.Haskeline
import           System.Directory
import           System.Environment
import           System.Exit ( die )
import           System.FilePath
import           Debug.Trace (traceM)
import qualified Syntax.Expression as E
import           Syntax.MkName


main :: IO ()
main = do
  args <- getArgs
  home <- (</> ".repl_history") <$> getHomeDirectory
  when (not (null args) && head args == "-v") (die replVersion)
  putStrLn $ replVersion ++ ": https://freest-lang.github.io/  :h for help"
  -- | Parse
  runFilePath <- getDataFileName "Prelude.fst"
  let s0 =  initialWithFile runFilePath 
  s2 <-  parseProgram s0
  -- | PatternMatch
  let patternS = patternMatch s2
  -- | Elaboration
  let (defs, elabS) = elaboration (pkVariables $ extra s2) (mVariables $ extra s2) patternS
  -- | Kind Inference  
  let infS = execState (renameProgram >> infer) (elabToInf defs elabS)  
  evalStateT (runInputT (replSettings home) (repl s2 args)) (infToTyping defaultOpts{runFilePath="<interactive>"} infS)

  -- s1 <- parseProgram (initialState {runOpts=defaultOpts{runFilePath}})
  -- let s2 = emptyPEnv $ execState elaboration s1
  -- evalStateT (runInputT (replSettings home) (repl s1 args))
  --   s2{runOpts=defaultOpts{runFilePath="<interactive>"}}

------------------------------------------------------------
-- AUTOCOMPLETE & HISTORY
------------------------------------------------------------

replSettings :: FilePath -> Settings REPLState
replSettings f = Settings
  { complete       = completeFun
  , historyFile    = Just f
  , autoAddHistory = True
  }

completeFun :: CompletionFunc REPLState
completeFun = completeWordWithPrev Nothing " " completionGenerator

completionGenerator :: String -> String -> REPLState [Completion]
completionGenerator "" "" = return []
completionGenerator "" opts = mkCompletion $ filter (opts `isPrefixOf`) optionList
completionGenerator " l:" suffix = lift $ listFiles suffix
completionGenerator " i:" suffix = do
  venv <- getSignatures
  tenv <- getTypes
  mkCompletion $ filter (suffix `isPrefixOf`) (convert venv ++ convert tenv)
  where
    convert :: Show a => Map.Map a b -> [String]
    convert = map show . Map.keys
completionGenerator _ suffix = do
  venv <- getSignatures
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

repl :: ParseS -> [String] -> InputT REPLState ()
repl s [] = handleInterrupt (repl s []) $
    withInterrupt $ getInputLine "λfreest> " >>= parseOpt s >> repl s []
repl s (x:_) = lift (load s x "OK. Module(s) loaded!") >> repl s []

-- | OPTIONS

type Option = Maybe String

parseOpt :: ParseS -> Option -> InputT REPLState ()
parseOpt _ Nothing  = liftS $ die "Leaving FreeSTi."
parseOpt s (Just xs) 
  | isOpt [":q", ":quit"] = liftS $ die "Leaving FreeSTi."
  | isOpt [":v", ":version"] = liftS $ putStrLn replVersion 
  | isOpt [":h", ":help"] = liftS $ putStrLn helpMenu
  | isOpt [":r", ":reload"] = lift $ reload s
  | opt == ":{" = multilineCmd s opt
  | isOpt [":l", ":load"] =  lift $ load s cont "OK. Module(s) loaded!"
  | isOpt [":t",":type"] = lift $ typeOf cont
  | isOpt [":k",":kind"] = lift $ kindOf cont
  | isOpt [":i", ":info"] = lift $ info cont
  | ":" `isPrefixOf` opt = liftS $ putStrLn $ "unknown command '" ++ opt ++ "', use :h for help"
  | isOpt ["data","type"] = do
      let s1 = parseDefs s "<interactive>" xs
      lift $ check s1 "<interactive>" Nothing
      s2 <- lift get
      runOpts <- lift Utils.getRunOpts
      if hasErrors s2
        then liftIO (putStrLn $ getErrors runOpts s2)
        else lift $ put s2
  | null opt = pure ()
  | otherwise = do
      st <- lift get
      runOpts <- lift Utils.getRunOpts
      case parseExpr "<interactive>" xs of
        Left err -> lift (setErrors err >> get) >>= \s0 -> liftIO (putStrLn $ getErrors runOpts s0)
        Right e       -> do
          s1 <- liftIO $ execStateT (T.synthetise Map.empty e) st
          if hasErrors s1
            then liftS $ putStrLn $ getErrors runOpts s1
            else liftS $ evalAndPrint (mkVar defaultSpan "main") st $
                  forkHandlers 
                    [ ("__runStdout", "__stdout")
                    , ("__runStderr", "__stderr")
                    , ("__runStdin", "__stdin")] 
                    e
  where
    (opt, cont) = splitOption xs
    isOpt = elem opt

    forkHandlers :: [(String, String)] -> E.Exp -> E.Exp
    forkHandlers [] e = e
    forkHandlers ((fun, var) : xs) e =
      E.UnLet s (mkWild s)
        (E.App s (E.Var s (mkFork s)) (E.App s (E.Var s (mkVar s fun)) (E.Var s (mkVar s var)))) 
        $ forkHandlers xs e 
      where
        s = defaultSpan
