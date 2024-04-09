{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module HandleOpts where

import           Elaboration.Elaboration ( elaboration )
import           FreeST
import           Interpreter.Value (Ctx)
import           Parse.Parser
import           Parse.Phase as PPh
import           Paths_FreeST (version)
import           PatternMatch.PatternMatch
import qualified Syntax.Base as B
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.Error
import           Util.GetTOps
import           Util.State as S
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
import Syntax.AST
import Typing.Phase (TypingS)
import Elaboration.Phase ( ElabS )
import PatternMatch.Phase (PatternS, Defs)
import qualified Typing.Phase as VP
import Interpreter.Eval (evaluate)

-- | -------------------------------------------------------
-- | Loads a file into the REPL
-- | Usage: `:l` or `:load`
-- | Details:
-- |   - It loses all the definitions made so far
-- |   - Deals with ~ in path
-- |   - Deals with FileNotFound & WrongFileExtension
-- | -------------------------------------------------------
load :: ParseS -> String -> String -> ReplState ()
load s ('~':'/':f) msg = do
  home <- lift getHomeDirectory
  setFilePath (home </> f)
  load' s (home </> f) msg
load s f msg = setFilePath f >> load' s f msg

load' :: ParseS -> String -> String -> ReplState ()
load' s f msg = freestLoadAndRun s f msg ("fst" `isExtensionOf` f) =<< lift (doesFileExist f)

freestLoadAndRun :: ParseS -> String -> String -> Bool -> Bool -> ReplState ()
freestLoadAndRun  _ f _ _ False = freestError $ FileNotFound f
freestLoadAndRun _ f _ False _ = freestError $ WrongFileExtension f
freestLoadAndRun s f msg _ _ = checkWithoutPrelude s f msg
--   wrapIO_ (parseAndImport (s{runOpts=defaultOpts{runFilePath=f}}))
--     $ unlessM (wrapExec $ elaboration >> stopPipeline (renameProgram >> typeCheck))
--          (lift $ putStrLn msg)

freestError :: ErrorType -> ReplState ()
freestError = lift . putStrLn . showError True "<FreeST>" Map.empty

-- | -------------------------------------------------------
-- | Reloads the previously loaded file
-- | Usage: `:r` or `:reload`
-- | Details:
-- |   - It looses all the definitions made so far
-- | -------------------------------------------------------

reload ::  ParseS -> ReplState ()
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
typeOf :: String -> ReplState ()
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
kindOf :: String -> ReplState ()
kindOf [] = lift $ putStrLn "syntax: ':k <type-to-synthetise-kind>'"
kindOf ts = do
  case parseType "<interactive>" ts of
    Left errors -> return () -- showErrors
    Right a@(T.Var _ x) -> getFromTypes x >>= synthVariable a x
    Right t -> K.synthetise Map.empty t >>= pretty ts
  where
    synthVariable :: T.Type -> B.Variable -> Maybe (K.Kind, T.Type) -> ReplState ()
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


info :: String -> ReplState ()
info [] = lift $ putStrLn "syntax: ':i <info-about>'"
info f@(x:_)
  | isUpper x = showInfo True ((ignoreFst <$>) . getFromTypes) (B.mkVar B.defaultSpan f)
  | otherwise = showInfo False getFromSignatures (B.mkVar B.defaultSpan f)

showInfo :: Bool -> (B.Variable -> ReplState (Maybe T.Type)) -> B.Variable -> ReplState ()
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
multilineCmd :: ParseS -> String -> InputT ReplState ()
multilineCmd _ xs' = do
  input <- liftS $ runInputT defaultSettings (readLoop (drop 2 xs'))
  -- let s0 = s{extra = (extra s){PPh.runOpts=(PPh.runOpts $ extra s){runFilePath="<interactive>"}, moduleName=Nothing}}
  handleProgram input

handleProgram :: String -> InputT ReplState ()
handleProgram input = do
  s <- lift get
  let s1 = parseDefs (initial initialExtraParse) "<interactive>" input
  -- liftS $ putStrLn (show (signatures (ast s1)))
  let ss = Map.union (signatures (ast s1)) (signatures (ast s))
      ts = Map.union (types (ast s1)) (types (ast s))
  runOpts <- lift Utils.getRunOpts
  let s2 = patternMatch s1{ast=(ast s1){signatures=ss,types=ts}}
  if hasErrors s2
  then liftIO (putStrLn $ getErrors runOpts s2)
  else let (defs,s3) = elaboration s2 in
    if hasErrors s3
    then liftIO (putStrLn $ getErrors runOpts s3)
    else let s4 = execState (renameProgram >> typeCheck) (elabToTyping runOpts defs s3) in
      if hasErrors s4 then liftIO (putStrLn $ getErrors runOpts s4)
      else do c <- ctx . extra <$> lift get
              c' <- liftS $ evaluate c s4
              lift (put s{ast=(ast s){signatures=signatures (ast s4)
                                     ,types=types (ast s4)
                                     }
                         ,extra=(extra s){ctx=c'}
                         }
                   )

readLoop :: String -> InputT IO String
readLoop s = getInputLine "freesti| " >>= \case
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


checkWithoutPrelude :: ParseS -> String -> String -> ReplState ()
checkWithoutPrelude prelude runFilePath successMsg = do
  -- | Parse
  s1 <- liftIO $ parseAndImport prelude{extra = (extra prelude){PPh.runOpts=interactiveRunOpts{runFilePath}}}
  if hasErrors s1
  then liftIO $ putStrLn $ getErrors interactiveRunOpts s1
  else let s2 = patternMatch s1 in
    if hasErrors s2
    then liftIO (putStrLn $ getErrors interactiveRunOpts s2)
    else let (defs,s3) = elaboration s2 in
      if hasErrors s3
      then liftIO (putStrLn $ getErrors interactiveRunOpts s3)
      else let runOpts = interactiveRunOpts{runFilePath}
               s4 = execState (renameProgram >> typeCheck) (elabToTyping runOpts defs s3) in
        if hasErrors s4
        then liftIO (putStrLn $ getErrors interactiveRunOpts s4)
        else do c <- gets (ctx . extra)
                c' <- lift $ evaluate c s4
                put $ typingToRepl c' s4

