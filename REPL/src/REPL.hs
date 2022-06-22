{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module REPL where

import           Elaboration.Elaboration ( elaboration )
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser
import           Syntax.Base
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.Error
import           Util.FreestState
-- import           Paths_FreeST ( getDataFileName )
import           Parse.Parser ( parseProgram, parseAndImport )
import qualified Validation.Kinding as K
import           Elaboration.Elaboration ( elaboration )

-- import           Interpreter.Builtin ( initialCtx )
import           Interpreter.Eval ( evalAndPrint )
-- import           Parse.Parser ( parseProgram, parseAndImport )
-- import           Util.FreestState
import           Util.GetTOps
import qualified Validation.Kinding as K
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )
import qualified Validation.Typing as T


import           Control.Monad.Extra
import           Control.Monad.State
import           Data.Char (isSpace, isUpper)
import           Data.Functor
import           Data.List
import qualified Data.Map.Strict as Map
import           Paths_FreeST ( getDataFileName )
import           System.Console.Haskeline
import           System.Directory
import           System.Exit ( die )
import           System.FilePath



type Option = Maybe String

type REPLState = StateT FreestS IO

main :: IO ()
main = do
  runFilePath <- getDataFileName "Prelude.fst"
  s1 <- parseProgram (initialState {runOpts=defaultOpts{runFilePath}})
  evalStateT (runInputT replSettings repl)  s1{runOpts=defaultOpts{runFilePath="<interactive>"}}

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
completionGenerator _ suffix = do
  venv <- getVEnv
  mkCompletion $ filter (suffix `isPrefixOf`) (map show $ Map.keys venv)

mkCompletion :: Applicative f => [String] -> f [Completion]
mkCompletion = pure . map (\s -> Completion s s False)

repl :: InputT REPLState ()
repl = getInputLine "λfreest> " >>= parseOpt >> repl

liftS :: IO a -> InputT (StateT FreestS IO) a
liftS = lift . lift 
  
parseOpt :: Option -> InputT REPLState ()
parseOpt Nothing  = return ()
parseOpt (Just xs)
  | xs' == ":q" || xs' == ":quit" = liftS $ die "Leaving FreeSTi."
  | xs' == ":h" || xs' == ":help" = liftS $ putStrLn $ helpMenu
  | xs' == ":r" || xs' == ":reload" = lift $ reload
  | xs' == ":{" = do
      input <- liftS $ runInputT defaultSettings (readLoop (drop 2 xs'))
      f <- lift getFileName
      st <- lift get
      let s1 = parseDefs st f input
      let s2 = emptyPEnv $ execState (elaboration >> renameState >> typeCheck) s1
      if hasErrors s2
        then liftS $ putStrLn $ getErrors s2
        else lift $ put s2
  | isOpt [":l ", ":load "] = lift $ load (opt xs')
  | isOpt [":t ",":type "] = lift $ typeOf (opt xs')
  | isOpt [":k ",":kind "] = lift $ kindOf (opt xs')
  | isOpt [":i ", ":info "] = lift $ info (opt xs')     
  | ":" `isPrefixOf` xs' = liftS $ putStrLn $ "unknown command '" ++ xs' ++ "', use :h for help"
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


helpMenu :: String
helpMenu = unlines
      ["-----------------------------------------------------------------------------------"
      ,"                          FreeST Help Menu                                         "
      ,"-----------------------------------------------------------------------------------"
      ,"Commands available from the prompt:\n"
      ,":help                     (:h)  Display the help menu"
      ,":quit                     (:q)  Quit FreeST"
      ,":{\\n ..lines.. \\n:}\\n        Multiline command, for function definitions"
      ,"<statement>                     Evaluate/Run <statement>"
      ,":load <filepath>          (:l)  Load an external file into the context"
      ,":type <expr>              (:t)  Display the type of an expression"
      ,":kind <type>              (:k)  Display the kind of a type"
      ,":info <name>              (:i)  Display information about the given name"
--      ,":module <filepath>        (:m)  Add file/module to the current context" -- IMPORT?
      ,":reload                   (:r)  Reload last file loaded into REPL"
      ,"-----------------------------------------------------------------------------------"
      ]


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

wrap :: FreestState a ->  REPLState Bool
wrap f = do
  s <- execState f <$> get
  if hasErrors s
    then lift (putStrLn $ getErrors s) $> True
    else put s $> False
  

load :: String -> REPLState ()
load f = do
  b1 <- not <$> lift (doesFileExist f)
  let b2 = not $ "fst" `isExtensionOf` f
  when b1 $ lift $ putStrLn fileDoNotExist
  when b2 $ lift $ putStrLn wrongFileExtension
  if (b1 && b2)
    then return ()
    else do
      s2 <- lift $ parseAndImport (initialState{runOpts=defaultOpts{runFilePath=f}})
      if hasErrors s2
        then lift $ putStrLn (getErrors s2)
        else do
          put s2
          unlessM (wrap (elaboration >> get >>= put . emptyPEnv)) $
            void $ wrap (renameState >> typeCheck)
  where
    fileDoNotExist = showErrors True "<FreeST>" Map.empty (FileNotFound f)
    wrongFileExtension = showErrors True "<FreeST>" Map.empty (WrongFileExtension f)



reload ::  REPLState ()
reload = do
  fp <- getFileName -- runFilePath (runOpts st)
  lift $ putStrLn fp
  let st' = initialState{ runOpts=defaultOpts{runFilePath=fp}} in
    if fp /= "<interactive>"
    then put st' >> load fp
    else lift $ putStrLn "No files loaded yet"


typeOf :: String -> REPLState ()
typeOf [] = lift $ putStrLn "syntax: ':t <expression-to-synthetise-type>'"
typeOf q = do
  let query = mkVar defaultSpan q in
    getFromVEnv query >>= \case
     Just t -> getTypeNames >>= \tn -> lift $ print $ getDefault tn t
     Nothing -> lift $ putStrLn $ q ++ " is not in scope."


-- TODO: elaborate after parsing
-- TODO: embellish 
kindOf :: String -> REPLState ()
kindOf [] = lift $ putStrLn "syntax: ':k <type-to-synthetise-kind>'"
kindOf ts = do
  case parseType "Prelude" ts of
    Left errors -> lift $ putStrLn (getErrors initialState{errors})
    Right a@(T.Var _ x) ->
      getFromVEnv x >>= \case
        Just t -> void $ wrapRun $ K.synthetise Map.empty t
        Nothing ->
          getFromTEnv x >>= \case
            Just (k,t) -> void $ wrapRun $ K.synthetise (Map.singleton x k) t
            Nothing -> void $ wrapRun $ K.synthetise Map.empty a
    Right t -> void $ wrapRun $ K.synthetise Map.empty t


wrapRun :: Show a => FreestState a ->  REPLState Bool
wrapRun f = do
  (v,s) <- runState f <$> get
  if hasErrors s
    then lift (putStrLn $ getErrors s) $> True
    else lift (print v) >> put s $> False


info :: String -> REPLState ()
info [] = lift $ putStrLn "syntax: ':i <info-about>'"
info f@(x:_)
  | isUpper x =
      let var = mkVar defaultSpan f in
        getFromTEnv var >>= \case 
          Nothing -> lift $ putStrLn ("Variable " ++ f ++ " is not in scope")
          Just (k,t) -- TODO: Join
            | isDatatype t -> do
                lift $ putStrLn $ "-- Defined at " ++ defModule (getSpan t) ++ ":" ++ show (getSpan t)
                               ++ "\ndata " ++ f ++ " : " ++ show k ++ " = " ++ showData var t
            | otherwise    -> do
                lift $ putStrLn $ "-- Defined at " ++ defModule (getSpan t) ++ ":" ++ show (getSpan t)
                               ++ "\ntype " ++ f ++ " : " ++ show k ++ " = " ++ showData var t
  | otherwise =
      let var = mkVar defaultSpan f in
        getFromVEnv var >>= \case
          Nothing -> lift $ putStrLn $ "Variable " ++ f ++ " is not in scope"
          Just t  -> do
            m <- getFromProg var
            let end = maybe (endPos $ getSpan t) (\e -> endPos $ getSpan e) m
            let loc = maybe (defModule $ getSpan t) (\e -> defModule $ getSpan e) m
            let s = Span (startPos $ getSpan t) end loc -- (defModule $ getSpan e)
            tn <- getTypeNames
            lift $ putStrLn $ "-- Defined at " ++ defModule (getSpan t) ++ ":" ++ show s ++ "\n" ++
                              f ++ " : " ++ show (getDefault tn t) ++ 
                              maybe "" (\e ->  "\n" ++ f ++ " = " ++ show (getDefault tn e)) m

  
showData :: Variable -> T.Type -> String
showData x (T.Rec _ b)
  | x == var b = showData x (body b)
showData _ t = show t


readLoop :: String -> InputT IO String
readLoop s = do
  input <- getInputLine " |λ> "
  case input of
    Just ":}" -> pure s
    Just str -> readLoop (s ++ "\n" ++ str)
    Nothing -> readLoop s
