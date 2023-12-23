{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module HandleOpts where

import           Elaboration.Elaboration ( elaboration )
import           FreeST
import           Parse.Parser
import           Parse.Phase
import           Paths_FreeST (version)
import           PatternMatch.PatternMatch
import qualified Syntax.Base as B
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.Error
import           Util.GetTOps
import           Util.State hiding (void)
import           Utils
import qualified Kinding.Kinding as K
import           Typing.Rename ( renameProgram )
import           Typing.Typing ( typeCheck )

import           Control.Monad.State
import           Data.Char (isUpper)
import qualified Data.Map.Strict as Map
import           Data.Version ( showVersion )
import           System.Console.Haskeline
import           System.Directory
import           System.FilePath

-- | -------------------------------------------------------
-- | Loads a file into the REPL
-- | Usage: `:l` or `:load`
-- | Details:
-- |   - It looses all the definitions made so far
-- |   - Deals with ~ in path
-- |   - Deals with FileNotFound & WrongFileExtension
-- | -------------------------------------------------------
load :: ParseS -> String -> String -> REPLState ()
load s ('~':'/':f) msg = do
  home <- lift getHomeDirectory
  setFilePath (home </> f)
  load' s (home </> f) msg
load s f msg = setFilePath f >> load' s f msg

load' :: ParseS -> String -> String -> REPLState ()
load' s f msg = freestLoadAndRun s f msg ("fst" `isExtensionOf` f) =<< lift (doesFileExist f)
  
freestLoadAndRun :: ParseS -> String -> String -> Bool -> Bool -> REPLState ()
freestLoadAndRun  _ f _ _ False = freestError $ FileNotFound f
freestLoadAndRun _ f _ False _ = freestError $ WrongFileExtension f
freestLoadAndRun s f msg _ _ = checkWithoutPrelude s f msg
--   wrapIO_ (parseAndImport (s{runOpts=defaultOpts{runFilePath=f}}))
--     $ unlessM (wrapExec $ elaboration >> stopPipeline (renameProgram >> typeCheck))
--          (lift $ putStrLn msg)

freestError :: ErrorType -> REPLState ()
freestError = lift . putStrLn . showError True "<FreeST>" Map.empty

-- | -------------------------------------------------------
-- | Reloads the previously loaded file
-- | Usage: `:r` or `:reload`
-- | Details:
-- |   - It looses all the definitions made so far
-- | -------------------------------------------------------

reload ::  ParseS -> REPLState ()
reload s = do
  fp <- getFilePath
  if fp /= "<interactive>"
    then load s fp "OK. Module(s) reloaded!"
    else lift $ putStrLn "No files loaded yet"

-- | -------------------------------------------------------
-- | Displays the type of a given expression 
-- | Usage: `:t <expr>` or `:type <expr>`
-- | It is used to display the type of some type.
-- | Shows the kind of:
-- |   - A command line expression: TODO (see below)
-- |   - A user defined function: `:t fun`
-- |   - A datatype constructor: `:t Node`
-- | -------------------------------------------------------

-- TODO: :t (\x:Int -> x * x)
typeOf :: String -> REPLState ()
typeOf [] = lift $ putStrLn "syntax: ':t <expression-to-synthetise-type>'"
typeOf q = do
  let query = B.mkVar B.defaultSpan q
  getFromSignatures query >>= \case
   Just t -> getTypeNames >>= \tn -> lift $ putStrLn $ q ++ " : " ++ show (getDefault tn t)
   Nothing -> lift $ putStrLn $ q ++ " is not in scope."

-- | -------------------------------------------------------
-- | Displays the kind of a given type 
-- | Usage: `:k <type>` or `:info <type>` 
-- | It is used to display the kind of some type.
-- | Shows the kind of:
-- |   - A command line type: `:k Int`
-- |   - A datatype or type abbreviation: `:k Tree`
-- | -------------------------------------------------------

-- TODO: elaborate after parsing?
kindOf :: String -> REPLState ()
kindOf [] = lift $ putStrLn "syntax: ':k <type-to-synthetise-kind>'"
kindOf ts = do  
  case parseType "<interactive>" ts of
    Left errors -> return () -- showErrors
    Right a@(T.Var _ x) -> getFromTypes x >>= synthVariable a x
    Right t -> K.synthetise Map.empty t >>= pretty ts
  where
    synthVariable :: T.Type -> B.Variable -> Maybe (K.Kind, T.Type) -> REPLState ()
    synthVariable _ x (Just (k,t)) =
      K.synthetise (Map.singleton x k) t >>= pretty ts
    synthVariable a x Nothing  =
      getFromSignatures x >>= \case
        Just t -> K.synthetise Map.empty t >>= pretty ts
        Nothing -> K.synthetise Map.empty a >>= pretty ts
      
-- | ----------------------------------------------------------
-- | Displays the available information for the given name.
-- | Usage: `:i <name>` or :info `<name>` 
-- | It is used to display the location of some type or function definition and
-- | also print the definition itself
-- | ----------------------------------------------------------


info :: String -> REPLState ()
info [] = lift $ putStrLn "syntax: ':i <info-about>'"
info f@(x:_)
  | isUpper x = showInfo True ((ignoreFst <$>) . getFromTypes) (B.mkVar B.defaultSpan f)
  | otherwise = showInfo False getFromSignatures (B.mkVar B.defaultSpan f)

showInfo :: Bool -> (B.Variable -> REPLState (Maybe T.Type)) -> B.Variable -> REPLState ()
showInfo b f var = f var >>= \case
  Nothing -> lift $ putStrLn $ "Variable " ++ show var ++ " is not in scope"
  Just t
    | b         -> lift $ putStrLn $ infoHeader (B.getSpan t) ++ infoData var t
    | otherwise -> do
        m <- getFromDefinitions var
        let (f, moduleName) = maybe defSpan mbLocExp m
            s = B.Span moduleName (B.startPos $ B.getSpan t) f
        getTypeNames >>= lift . putStrLn . (infoHeader s ++) . infoFun var t m
     where
       defSpan = (B.endPos $ B.getSpan t, B.moduleName $ B.getSpan t)
       mbLocExp e  = let se = B.getSpan e in (B.endPos se, B.moduleName se)

infoHeader :: B.Span -> String
infoHeader s = "-- Defined at " ++ B.moduleName s ++ ":" ++ show s

infoData :: B.Variable -> T.Type -> String
infoData x t = (if isDatatype t then "\ndata " else "\ntype ")
                ++ show x ++ " = " ++ showData x t
  where
    showData :: B.Variable -> T.Type -> String
    showData x (T.Rec _ b)
      | x == B.var b = " : " ++ show (B.binder b) ++ " = " ++ showData x (B.body b)
    showData _ t = show t
    
infoFun :: B.Variable -> T.Type -> Maybe E.Exp -> TypeOpsEnv -> String
infoFun var t mbe tn = "\n" ++ show var ++ " : " ++ show (getDefault tn t) ++ 
      maybe "" (\e ->  "\n" ++ show var ++ " = " ++ show (getDefault tn e)) mbe

-- | -------------------------------------------------------
-- | Handles a multiline command.
-- | Usage: `:{\n ..lines.. \n:}\n`
-- | It is usually used to define functions, datatypes, or type abbreviations
-- | -------------------------------------------------------
-- {extra = (extra s){runOpts=interactiveRunOpts{runFilePath="<interactive>"}}}
multilineCmd :: ParseS -> String -> InputT REPLState ()
multilineCmd s xs' = do
  input <- liftS $ runInputT defaultSettings (readLoop (drop 2 xs'))
  let s0 = s{extra = (extra s){runOpts=(runOpts $ extra s){runFilePath="<interactive>"}, moduleName=Nothing}}
  let s1 = parseDefs s0 "<interactive>" input
  lift $ check s1 "<interactive>" Nothing
  s2 <- lift get
  runOpts <- lift Utils.getRunOpts
  if hasErrors s2
    then liftS $ putStrLn $ getErrors runOpts s2
    else lift $ put s2

readLoop :: String -> InputT IO String
readLoop s = getInputLine " |λ> " >>= \case
  Just ":}" -> pure s
  Just str -> readLoop (s ++ "\n" ++ str)
  Nothing -> readLoop s

-- | -------------------------------------------------------
-- | The help menu
-- | Usage: `:h` or `:help`
-- | Displays the REPL's help menu
-- | -------------------------------------------------------

helpMenu :: String
helpMenu = replVersion ++ "\n\n" ++ unlines
      ["-----------------------------------------------------------------------------------"
      ,"                          FreeSTi Help Menu                                        "
      ,"-----------------------------------------------------------------------------------"
      ,"Commands available from the prompt:\n"
      ,":help                     (:h)  Display the help menu"
      ,":quit                     (:q)  Quit FreeST"
      ,":{\\n ..lines.. \\n:}\\n           Multiline command, for function definitions"
      ,"<statement>                     Evaluate/Run <statement>"
      ,":load <filepath>          (:l)  Load an external file into the context"
      ,":type <expr>              (:t)  Display the type of an expression"
      ,":kind <type>              (:k)  Display the kind of a type"
      ,":info <name>              (:i)  Display information about the given name"
--      ,":module <filepath>        (:m)  Add file/module to the current context" -- IMPORT?
      ,":reload                   (:r)  Reload last file loaded into REPL"
      ,"-----------------------------------------------------------------------------------"
      ]


replVersion :: String
replVersion = "FreeSTi, version " ++ showVersion version ++ if isDev then "-dev" else ""


checkWithoutPrelude :: ParseS -> String -> String -> REPLState ()
checkWithoutPrelude prelude runFilePath successMsg = do
  -- | Parse
  s <- liftIO $ parseAndImport prelude{extra = (extra prelude){runOpts=interactiveRunOpts{runFilePath}}}
  if hasErrors s
    then liftIO $ putStrLn $ getErrors interactiveRunOpts s
    else check s runFilePath (Just successMsg)

check :: ParseS -> String -> Maybe String -> REPLState () 
check s runFilePath successMsg = do
  let runOpts = interactiveRunOpts{runFilePath}
  let patternS = patternMatch s
  if hasErrors patternS
    then liftIO $ putStrLn $ getErrors runOpts patternS
    else let (defs, elabS) = elaboration patternS in
      if hasErrors elabS
      then liftIO $ putStrLn $ getErrors runOpts elabS
      else            
        let s4 = execState (renameProgram >> typeCheck) (elabToTyping runOpts{runFilePath} defs elabS) in
        if hasErrors s4 then liftIO $ putStrLn $ getErrors runOpts s4
        else put s4 >>
          maybe (return ()) (liftIO . putStrLn) successMsg
