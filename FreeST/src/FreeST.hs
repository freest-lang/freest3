{-# LANGUAGE NamedFieldPuns #-}
module FreeST
  ( main
  , checkAndRun
  )
where


import           Elaboration.Elaboration ( elaboration )
import           Interpreter.Builtin ( initialCtx, new )
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser ( parseProgram, parseAndImport )
import           Util.CmdLine
import           Util.FreestState
import           Util.PreludeLoader ( prelude )
import           Util.Warning
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )

import           Control.Monad.State ( when, execState )
import qualified Data.Map.Strict as Map
import           Paths_FreeST ( getDataFileName )
import           System.Exit ( die )

import Syntax.Base
import qualified Syntax.Kind as K
import Syntax.Type
import Interpreter.Value
import qualified Syntax.Expression as E

main :: IO ()
main = checkAndRun =<< flags -- handleOpts =<< compilerOpts =<< getArgs

checkAndRun :: RunOpts -> IO ()
checkAndRun runOpts = do
  -- | Prelude
  preludeFp <- getDataFileName "Prelude.fst"
  let s0 = initialState {varEnv = prelude, runOpts=runOpts{runFilePath=preludeFp}}
  s1 <- preludeHasErrors (runFilePath runOpts) s0 <$> (parseProgram s0)

  -- | Parse
  s2 <- parseAndImport s1{runOpts}
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

 -- | Check if main was left undefined, eval and print result otherwise
  let m = getMain runOpts
  when (m `Map.member` varEnv s5) $
    addPrimitiveChannels ["stdout", "stdin", "stderr"] initialCtx >>= \ctx ->
    (evalAndPrint 
      (typeEnv s5) 
      (ctx)
      (prog s5)
      (forkHandlers [("#runStdout", "#stdout"), ("#runStdIn", "#stdin")] (prog s5 Map.! m))
    )

  where
    preludeHasErrors :: FilePath -> FreestS -> FreestS -> FreestS
    preludeHasErrors f s0 s1
      | hasErrors s1 = s0 { warnings = NoPrelude f : warnings s0 }
      | otherwise    = s1

    addPrimitiveChannels :: [String] -> Ctx -> IO Ctx
    addPrimitiveChannels [] ctx = return ctx
    addPrimitiveChannels (varName : varNames) ctx = do
      (clientChan, serverChan) <- new
      addPrimitiveChannels varNames 
        $ Map.insert (mkVar defaultSpan         varName ) (Chan clientChan) 
        $ Map.insert (mkVar defaultSpan ("#" ++ varName)) (Chan serverChan) ctx

    forkHandlers :: [(String, String)] -> E.Exp -> E.Exp
    forkHandlers [] e = e
    forkHandlers ((fun, var) : xs) e =
      E.UnLet s (mkVar s "_") 
        (E.App s (E.Var s (mkVar s "fork")) (E.App s (E.Var s (mkVar s fun)) (E.Var s (mkVar s var)))) 
        $ forkHandlers xs e 
      where
        s = defaultSpan