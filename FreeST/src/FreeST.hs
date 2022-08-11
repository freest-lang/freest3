{-# LANGUAGE NamedFieldPuns #-}
module FreeST
  ( main
  , checkAndRun
  )
where


import           Elaboration.Elaboration ( elaboration )
import           Interpreter.Builtin ( initialCtx, new )
import           Interpreter.Eval ( evalAndPrint )
import           Interpreter.Value
import           Parse.Parser ( parseProgram, parseAndImport )
import           Util.CmdLine
import           Util.FreestState
import           Syntax.Base
import           Syntax.Program (noConstructors, VarEnv)
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Util.Error
import           Util.Warning
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )

import           Control.Monad.State ( when, unless, execState )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Paths_FreeST ( getDataFileName )
import           System.Exit ( die )


main :: IO ()
main = checkAndRun =<< flags -- handleOpts =<< compilerOpts =<< getArgs

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
  when (m `Map.member` varEnv s5) $
    addPrimitiveChannels ["stdout", "stdin", "stderr"] initialCtx >>= \ctx ->
    (evalAndPrint 
      (typeEnv s5) 
      (ctx)
      (prog s5)
      (forkHandlers [("runStdout", "_stdout"), ("runStderr", "_stderr"), ("runStdin", "_stdin")] (prog s5 Map.! m))
    )

  where
    noSig :: VarEnv -> Variable -> FreestS -> FreestS
    noSig venv f acc = acc { errors = SignatureLacksBinding (getSpan f) f (venv Map.! f) : errors acc }
      
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
        $ Map.insert (mkVar defaultSpan ("_" ++ varName)) (Chan serverChan) ctx

    forkHandlers :: [(String, String)] -> E.Exp -> E.Exp
    forkHandlers [] e = e
    forkHandlers ((fun, var) : xs) e =
      E.UnLet s (mkVar s "_") 
        (E.App s (E.Var s (mkVar s "fork")) (E.App s (E.Var s (mkVar s fun)) (E.Var s (mkVar s var)))) 
        $ forkHandlers xs e 
      where
        s = defaultSpan