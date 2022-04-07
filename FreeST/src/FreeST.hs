{-# LANGUAGE NamedFieldPuns #-}
module FreeST
  ( main
  , checkAndRun
  )
where


import           Elaboration.Elaboration ( elaboration )
import           Interpreter.Builtin ( initialCtx )
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser ( parseProgram, parseAndImport )
import           Syntax.Program
import           Util.PrettyError
import           Util.ErrorMessage
import           Util.FreestState
import           Util.PreludeLoader ( prelude , userDefined )
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )
import           Util.CmdLine


import           Control.Monad.State ( when, execState )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Paths_FreeST ( getDataFileName )
import           System.Exit ( die )

main :: IO ()
main = checkAndRun =<< flags -- handleOpts =<< compilerOpts =<< getArgs

checkAndRun :: RunOpts -> IO ()
checkAndRun runOpts = do
  -- No file, die
  -- when (isNothing $ runFilePath runOpts) (die "")
  -- Prelude
  -- preludeFilePath <- getDataFileName (fromJust $ preludeFile runOpts)
  preludeFilePath <- getDataFileName "Prelude.fst"
  s0              <- parseProgram initialState preludeFilePath prelude -- TODO: review and add things to the initial state
  when (hasErrors s0) (putStrLn cantFindPrelude)
  let (initialPrelude, initialProg) = fromPreludeFile s0
  let preludeNames = Map.keys initialPrelude
  -- Parse
  s1 <- parseAndImport s0 (runFilePath runOpts) initialPrelude
--  s1 <- parseProgram (runFilePath runOpts) initialPrelude
  when (hasErrors s1) (die $ getErrors preludeNames s1)
  -- putStrLn $ "\t\t   " ++ show (Map.withoutKeys (userDefined $ varEnv s1) (Set.fromList preludeNames))
  -- Solve type declarations and dualof operators
  let s2 = emptyPEnv $ execState
        elaboration
        (s1 { runOpts, parseEnv = parseEnv s1 `Map.union` initialProg})
  when (hasErrors s2) (die $ getErrors preludeNames s2)
  -- Rename
  let s3 = execState renameState (s2 { runOpts })
  -- Type check
  let s4 = execState typeCheck (s3 { runOpts })
  when (not (quietmode runOpts) && hasWarnings s4) (putStrLn $ getWarnings s4)
  when (hasErrors s4)   (die $ getErrors preludeNames s4)
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
