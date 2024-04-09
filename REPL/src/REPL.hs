{-# LANGUAGE NamedFieldPuns #-}
module REPL where

import           Elaboration.Elaboration ( elaboration )
import           FreeST hiding (main)
import           Interpreter.Builtin (initialCtx)
import           Interpreter.Eval ( evalAndPrint, evaluate, evalAndPrint' )
import           HandleOpts
import           Parse.Parser
import           Paths_FreeST ( getDataFileName )
import           Syntax.Base
import           Syntax.AST
import Syntax.Expression(Pattern)
import           Util.State
import           Utils
import qualified Typing.Typing as T
import           Parse.Phase
import           PatternMatch.PatternMatch

import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Console.Haskeline
import           System.Directory
import           System.Environment
import           System.Exit ( die )
import           System.FilePath
import Elaboration.Replace (Replace(replace))
import Typing.Rename (Rename(rename), renameProgram)


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
  let (defs, elabS) = elaboration patternS
  let typingS = elabToTyping defaultOpts{runFilePath="<interactive>"} defs elabS
  c <- evaluate initialCtx typingS
  evalStateT (runInputT (replSettings home) (repl s2 args)) (typingToRepl c typingS)

  -- s1 <- parseProgram (initialState {runOpts=defaultOpts{runFilePath}})
  -- let s2 = emptyPEnv $ execState elaboration s1
  -- evalStateT (runInputT (replSettings home) (repl s1 args))
  --   s2{runOpts=defaultOpts{runFilePath="<interactive>"}}

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
              s0 = patternMatch st{ast=(ast st){signatures=Map.insert it' (omission defaultSpan) (signatures $ ast st)
                                                 ,definitions=Map.singleton it' [([],e)]}
                                  ,extra=extra s}
          if hasErrors s0
          then liftIO (putStrLn $ getErrors runOpts s0)
          else let (defs,s1) = elaboration s0 in 
            if hasErrors s1
            then liftIO (putStrLn $ getErrors runOpts s1)
            else let s1' = execState renameProgram (elabToTyping runOpts defs s1)
                     e'  = definitions (ast s1') Map.! it'
                     (t,s2) = runState (T.synthetise Map.empty e') st in 
              if hasErrors s2 
              then liftIO (putStrLn $ getErrors runOpts s2)
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



