{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module HandleOpts where

import           Elaboration.Elaboration ( elaboration )
import           Parse.Parser
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.Error
import           Util.FreestState
import           Util.GetTOps
import           Utils
import qualified Validation.Kinding as K
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )


import           Control.Monad.Extra
import           Control.Monad.State
import           Data.Char (isUpper)
import qualified Data.Map.Strict as Map
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
load :: FreestS -> String -> String -> REPLState ()
load s ('~':'/':f) msg = do
  home <- lift getHomeDirectory
  load' s (home </> f) msg
load s f msg = load' s f msg

load' :: FreestS -> String -> String -> REPLState ()
load' s f msg = 
  freestLoadAndRun s f msg ("fst" `isExtensionOf` f) =<< lift (doesFileExist f)
  
freestLoadAndRun :: FreestS -> String -> String -> Bool -> Bool -> REPLState ()
freestLoadAndRun  _ f _ _ False = freestError $ FileNotFound f
freestLoadAndRun _ f _ False _ = freestError $ WrongFileExtension f
freestLoadAndRun s f msg _ _ = do
  wrapIO_ (parseAndImport (s{runOpts=defaultOpts{runFilePath=f}}))
    $ unlessM (wrapExec $ elaboration >> stopPipeline (renameState >> typeCheck))
         (lift $ putStrLn msg)

freestError :: ErrorType -> REPLState ()
freestError = lift . putStrLn . showErrors True "<FreeST>" Map.empty

-- | -------------------------------------------------------
-- | Reloads the previously loaded file
-- | Usage: `:r` or `:reload`
-- | Details:
-- |   - It looses all the definitions made so far
-- | -------------------------------------------------------

reload ::  FreestS -> REPLState ()
reload s = do
  fp <- getFileName
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
  let query = mkVar defaultSpan q in
    getFromVEnv query >>= \case
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
    Left errors -> lift $ putStrLn (getErrors initialState{errors})
    Right a@(T.Var _ x) -> getFromTEnv x >>= synthVariable a x
    Right t -> void $ wrapRun $ K.synthetise Map.empty t
  where
    synthVariable :: T.Type -> Variable -> Maybe (K.Kind, T.Type) -> REPLState ()
    synthVariable _ x (Just (k,t)) = void $ wrapRun $ K.synthetise (Map.singleton x k) t
    synthVariable a _ Nothing  = void $ wrapRun $ K.synthetise Map.empty a



-- | ----------------------------------------------------------
-- | Displays the available information for the given name.
-- | Usage: `:i <name>` or :info `<name>` 
-- | It is used to display the location of some type or function definition and
-- | also print the definition itself
-- | ----------------------------------------------------------


info :: String -> REPLState ()
info [] = lift $ putStrLn "syntax: ':i <info-about>'"
info f@(x:_)
  | isUpper x = showInfo True ((ignoreFst <$>) . getFromTEnv) (mkVar defaultSpan f)
  | otherwise = showInfo False getFromVEnv (mkVar defaultSpan f)

showInfo :: Bool -> (Variable -> REPLState (Maybe T.Type)) -> Variable -> REPLState ()
showInfo b f var = f var >>= \case
  Nothing -> lift $ putStrLn $ "Variable " ++ show var ++ " is not in scope"
  Just t
    | b         -> lift $ putStrLn $ infoHeader (getSpan t) ++ infoData var t
    | otherwise -> do
        m <- getFromProg var
        let s = uncurry (Span (startPos $ getSpan t)) (maybe defSpan mbLocExp m)
        getTypeNames >>= lift . putStrLn . (infoHeader s ++) . infoFun var t m
     where
       defSpan = (endPos $ getSpan t, defModule $ getSpan t)
       mbLocExp e  = let se = getSpan e in (endPos se, defModule se)

infoHeader :: Span -> String
infoHeader s = "-- Defined at " ++ defModule s ++ ":" ++ show s

infoData :: Variable -> T.Type -> String
infoData x t = (if isDatatype t then "\ndata " else "\ntype ")
                ++ show x ++ showData x t
  where
    showData :: Variable -> T.Type -> String
    showData x (T.Rec _ b)
      | x == var b = " : " ++ show (binder b) ++ " = " ++ showData x (body b)
    showData _ t = show t
    
infoFun :: Variable -> T.Type -> Maybe E.Exp -> TypeOpsEnv -> String
infoFun var t mbe tn = "\n" ++ show var ++ " : " ++ show (getDefault tn t) ++ 
      maybe "" (\e ->  "\n" ++ show var ++ " = " ++ show (getDefault tn e)) mbe

-- | -------------------------------------------------------
-- | Handles a multiline command.
-- | Usage: `:{\n ..lines.. \n:}\n`
-- | It is usually used to define functions, datatypes, or type abbreviations
-- | -------------------------------------------------------

multilineCmd :: String -> InputT REPLState ()
multilineCmd xs' = do
  input <- liftS $ runInputT defaultSettings (readLoop (drop 2 xs'))
  f <- lift getFileName
  st <- lift get
  let s1 = parseDefs st f input
  let s2 = emptyPEnv $ execState (elaboration >> renameState >> typeCheck) s1
  if hasErrors s2
    then liftS $ putStrLn $ getErrors s2
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
helpMenu = unlines
      ["-----------------------------------------------------------------------------------"
      ,"                          FreeST Help Menu                                         "
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
