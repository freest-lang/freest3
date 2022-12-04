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
import           Util.CmdLine
import           Util.FreestState
import           Syntax.Base
import           Syntax.Program (noConstructors, VarEnv)
import           Util.Error
import           Util.Warning
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )

import           Control.Monad.State ( when, unless, execState )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Paths_FreeST ( getDataFileName )
import           System.Exit ( die )

isDev :: Bool
isDev = True

main :: IO ()
main = checkAndRun =<< flags isDev -- handleOpts =<< compilerOpts =<< getArgs

checkAndRun :: RunOpts -> IO ()
checkAndRun runOpts = do
  -- | Prelude
  preludeFp <- getDataFileName "Prelude.fst"
  let s0 = initialState {runOpts=runOpts{runFilePath=preludeFp}}
  s1 <- preludeHasErrors (runFilePath runOpts) s0 <$> parseProgram s0

  -- | Prelude entries without body are builtins  
  let venv = Map.keysSet (noConstructors (typeEnv s1) (varEnv s1))
  let penv = Map.keysSet (parseEnv s1)
  let bs = Set.difference venv penv

  -- | Parse
  s2 <- parseAndImport s1{builtins=bs, runOpts}
  when (hasErrors s2) (die $ getErrors s2)

 -- | Solve type declarations and dualof operators
  let s3 = emptyPEnv $ execState elaboration s2
  when (hasErrors s3) (die $ getErrors s3)

 -- | Rename
  let s4 = execState renameState s3

 -- | Type check
  let s5 = execState typeCheck s4
  when (not (quietmode runOpts) && hasWarnings s5) (putStrLn $ getWarnings s5)
  when (hasErrors s5)  (die $ getErrors s5)

 -- | Check whether a given function signature has a corresponding
 --   binding
  let venv = Map.keysSet (noConstructors (typeEnv s5) (varEnv s5))
  let p = Map.keysSet (prog s5)
  let bs = Set.difference (Set.difference venv p) (builtins s5)
  
  unless (Set.null bs) $
    die $ getErrors $ Set.foldr (noSig (varEnv s5)) initialState bs
  
 -- | Check if main was left undefined, eval and print result otherwise
  let m = getMain runOpts
  when (m `Map.member` varEnv s5) $ evalAndPrint m s5 (prog s5 Map.! m)
  -- when (m `Map.member` varEnv s5)
  --   (evalAndPrint (typeEnv s5) initialCtx
  --   (prog s5)
  --   (prog s5 Map.! m))

  where
    noSig :: VarEnv -> Variable -> FreestS -> FreestS
    noSig venv f acc = acc { errors = SignatureLacksBinding (getSpan f) f (venv Map.! f) : errors acc }
      
    preludeHasErrors :: FilePath -> FreestS -> FreestS -> FreestS
    preludeHasErrors f s0 s1
      | hasErrors s1 = s0 { warnings = NoPrelude f : warnings s0 }
      | otherwise    = s1
