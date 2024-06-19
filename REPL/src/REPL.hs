module REPL where

import           Elaboration.Elaboration ( elaboration )
import           Elaboration.Replace (Replace(replace))
import           FreeST hiding (main)
import           Interpreter.Builtin (initialCtx)
import           HandleOpts
import           Inference.Inference
import           Interpreter.Eval ( evalAndPrint, evalAndPrint', evaluate )
import           Parse.Parser
import           Parse.Phase
import           Paths_FreeST ( getDataFileName )
import           PatternMatch.PatternMatch
import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.MkName
import           Typing.Rename (Rename(rename), renameProgram )
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
  let (defs, elabS) = elaboration (pkVariables $ extra s2) (mVariables $ extra s2) patternS
  let infS = execState (renameProgram >> infer) (elabToInf defs elabS) 
  let typingS = infToTyping defaultOpts{runFilePath="<interactive>"} infS
  c <- evaluate initialCtx typingS
  evalStateT (runInputT (replSettings home) (repl s2 args)) (typingToRepl c typingS)

------------------------------------------------------------
-- AUTOCOMPLETE & HISTORY
------------------------------------------------------------

replSettings :: FilePath -> Settings ReplState
replSettings f = Settings
  { complete       = completeFun
  , historyFile    = Just f
  , autoAddHistory = True
  }

completeFun :: CompletionFunc ReplState
completeFun = completeWordWithPrev Nothing " " completionGenerator

completionGenerator :: String -> String -> ReplState [Completion]
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

repl :: ParseS -> [String] -> InputT ReplState ()
repl s [] = handleInterrupt (repl s []) $
    withInterrupt $ getInputLine "freesti> " >>= parseOpt s >> repl s []
repl s (x:_) = lift (load s x "OK. Module(s) loaded!") >> repl s []

-- | OPTIONS

type Option = Maybe String

parseOpt :: ParseS -> Option -> InputT ReplState ()
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
  | isOpt ["data","type"] = handleProgram xs
  | null opt = pure ()
  | otherwise = do
      runOpts <- lift Utils.getRunOpts
      case parseExpr "<interactive>" xs of
        Left err -> do
          s0 <- lift (setErrors err >> get)
          liftIO (putStrLn $ getErrors runOpts s0)
          lift (setErrors [])
        Right e  -> do 
          (st,ctx) <- replToTyping <$> lift get 
          let it' = mkVar defaultSpan "#it"
              s0 = st{ast=(ast st){signatures=Map.insert it' (omission defaultSpan) (signatures $ ast st)
                                                 ,definitions=Map.singleton it' [([],e)]}
                                  ,extra=extra s}
              s1 = patternMatch s0
          if hasErrors s1
          then liftIO (putStrLn $ getErrors runOpts s1)
          else let (defs,s2) = elaboration (pkVariables $ extra s0) (mVariables $ extra s0) s1 in 
            if hasErrors s2
            then liftIO (putStrLn $ getErrors runOpts s2)
            else let s2' = execState (renameProgram >> infer) (elabToInf defs s2) in 
              if hasErrors s2'
              then liftIO (putStrLn $ getErrors runOpts s2')
              else do
                let e' = definitions (ast s2') Map.! it'
                (t,s3) <- liftIO $ runStateT (T.synthetise Map.empty e') st
                if hasErrors s3 
                then liftIO (putStrLn $ getErrors runOpts s3)
                else do 
                  let st'=st{ast=(ast st){ evalOrder   = [[it']]
                                       , definitions = Map.insert it' e' (definitions (ast st))
                                       }
                          }
                  (v,ctx') <- liftS $ evalAndPrint' it' ctx st'
                  let it = mkVar defaultSpan "it"
                  lift $ put (typingToRepl (Map.insert it v ctx') st{ast=addSignature it t(ast st)})
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
