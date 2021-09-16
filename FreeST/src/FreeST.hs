{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module FreeST
  ( main
  , checkAndRun
  )
where

import           Control.Monad.State            ( when
                                                , unless
                                                , execState
                                                )
import qualified Data.Map.Strict               as Map
import           Elaboration.Elaboration        ( elaboration )
import           Interpreter.Builtin            ( initialCtx )
import           Interpreter.Eval               ( evalAndPrint )
import           Parse.Parser                   ( parseProgram )
import           Syntax.Base
import           Syntax.Program
import           System.Directory
import           System.Environment             ( getArgs )
import           System.Exit                    ( die )
import           System.FilePath
import           Util.PrettyError
import           Util.ErrorMessage
import           Util.FreestState
import           Util.PreludeLoader             ( prelude )
import           Validation.Rename              ( renameState )
import           Validation.TypeChecking        ( typeCheck )
import           Data.Version                   ( showVersion )
import           Paths_FreeST                   ( version
                                                , getDataFileName
                                                )
import           Data.Maybe
import           Util.CmdLine

main :: IO ()
main = checkAndRun =<< handleOpts =<< compilerOpts =<< getArgs

checkAndRun :: RunOpts -> IO ()
checkAndRun runOpts = do
  -- No file, die
  when (isNothing $ runFilePath runOpts) (die "")
  -- Prelude
  -- preludeFilePath <- getDataFileName (fromJust $ preludeFile runOpts)
  preludeFilePath <- getDataFileName "Prelude.fst"
  s0              <- parseProgram preludeFilePath prelude
  when (hasErrors s0) (putStrLn cantFindPrelude)
  let (initialPrelude, initialProg) = fromPreludeFile s0
  -- Parse
  s1 <- parseProgram (fromJust $ runFilePath runOpts) initialPrelude
  when (hasErrors s1) (die $ getErrors s1)
  -- Solve type declarations and dualof operators
  let s2 = emptyPEnv $ execState
        elaboration
        (s1 { runOpts, parseEnv = parseEnv s1 `Map.union` initialProg })
  when (hasErrors s2) (die $ getErrors s2)
  -- Rename
  let s3 = execState renameState (s2 { runOpts })
  -- Type check
  let s4 = execState typeCheck (s3 { runOpts })
  when (not (isQuietFlagSet runOpts) && hasWarnings s4) (putStrLn $ getWarnings s4)
  when (hasErrors s4)   (die $ getErrors s4)
  -- Check if main was left undefined
  let main = getMain runOpts
  -- If main is defined, eval and print
  when (main `Map.member` varEnv s4)
    (evalAndPrint (typeEnv s4) initialCtx
    (prog s4)
    (prog s4 Map.! main))
 where
  fromPreludeFile :: FreestS -> (VarEnv, ParseEnv)
  fromPreludeFile s0 | hasErrors s0 = (prelude, Map.empty)
                     | otherwise    = (varEnv s0, parseEnv s0)
  -- TODO: remove later (proper warnings)
  cantFindPrelude :: String
  cantFindPrelude =
    formatColor (Just Cyan) $ "warning: " ++ "Couldn't find prelude; proceeding without it"
