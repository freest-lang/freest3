{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}
module FreeST
  ( main
  , checkAndRun
  , isDev
  )
where


import           Elaboration.Elaboration -- ( elaboration )
import           Elaboration.Phase
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser ( parseProgram, parseAndImport )
import           Parse.Phase
import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.MkName
import           Syntax.Program (noConstructors)
import           Util.CmdLine
import           Util.Error
import           Util.State.State
import           Util.Warning
import           Validation.Phase
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )

import           Control.Monad.State hiding (void)
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
  s0 <- initialWithFile <$> getDataFileName "Prelude.fst"
  s1 <- preludeHasErrors (runFilePath runOpts) s0 <$> parseProgram s0
  -- | Prelude entries without body are builtins  
  let sigs = Map.keysSet (noConstructors (getTypesS s1) (getSignaturesS s1))
  let penv = Map.keysSet (getDefsS s1)
  let bs = Set.difference sigs penv

  -- | Parse
--  s2 <- parseAndImport s1{builtins=bs, runOpts}
  s2 <- parseAndImport s1{extra = (extra s1){runOpts}}
  when (hasErrors s2) (die $ getErrors runOpts s2)

  let (oldDef, s2') = parseToElab s2

  -- | Solve type declarations and dualof operators
  let (prog, s3) = runState (elaboration oldDef (parseToPattern s2)) s2'
  when (hasErrors s3) (die $ getErrors runOpts s3)
  
  -- | Rename
  let s4 = execState renameState (elabToTyping prog s3)

  -- | Type check
  let s5 = execState (typeCheck runOpts) s4
  when (not (quietmode runOpts) && hasWarnings s5) (putStrLn $ getWarnings runOpts s5)
  when (hasErrors s5)  (die $ getErrors runOpts s5)

  -- | Check whether a given function signature has a corresponding
  --   binding
  let sigs = Map.keysSet (noConstructors (types $ ast s5) (signatures $ ast s5))
  let p = Map.keysSet (definitions $ ast s5)
  let bs1 = Set.difference (Set.difference sigs p) bs -- (builtins s5)

  unless (Set.null bs1) $
    die $ getErrors runOpts $ initialS {errors = Set.foldr (noSig (getSignaturesS s5)) [] bs1}
  
  -- | Check if main was left undefined, eval and print result otherwise
  let m = getMain runOpts

  
  when (m `Map.member` getSignaturesS s5) $ evalAndPrint m s5 $
    forkHandlers 
      [ ("__runStdout", "__stdout")
      , ("__runStderr", "__stderr")
      , ("__runStdin", "__stdin")] 
      (getDefsS s5 Map.! m)

  where
    noSig :: Signatures -> Variable -> Errors -> Errors
--    noSig sigs f acc = acc { errors = SignatureLacksBinding (getSpan f) f (sigs Map.! f) : errors acc }
    noSig sigs f acc = SignatureLacksBinding (getSpan f) f (sigs Map.! f) : acc
      
    preludeHasErrors :: FilePath -> FreestS Parse -> FreestS Parse -> FreestS Parse
    preludeHasErrors f s0 s1
      | hasErrors s1 = s0 { warnings = NoPrelude f : warnings s0 }
      | otherwise    = s1

    forkHandlers :: [(String, String)] -> E.Exp -> E.Exp
    forkHandlers [] e = e
    forkHandlers ((fun, var) : xs) e =
      E.UnLet s (mkWild s)
        (E.App s (E.Var s (mkFork s)) (E.App s (E.Var s (mkVar s fun)) (E.Var s (mkVar s var)))) 
        $ forkHandlers xs e 
      where
        s = defaultSpan



parseToPattern :: FreestParse -> FreestPattern
parseToPattern s = s {ast = (ast s){definitions = Map.empty}, extra = void}


parseToElab :: FreestParse -> (Definitions Parse, FreestElab)
parseToElab s = (definitions $ ast s, s {ast=newAst, extra = void})
  where newAst = initialAST{types=types $ ast s, signatures=signatures $ ast s}

  -- (definitions $ ast s, s {ast=newAst, nextIndex=nextIndex s,
  --                                errors=errors s, warnings=warnings s,
  --                                typenames=typenames s,
  --                                extra = void})


elabToTyping :: Map.Map Variable E.Exp -> FreestElab -> FreestTyping
elabToTyping defs s = s {ast=newAst, extra = void}
  where newAst = AST {types=types $ ast s, signatures=signatures $ ast s, definitions = defs}

-- elabToTyping defs s = s {ast=newAst, nextIndex=nextIndex s,
--                           errors=errors s, warnings=warnings s,
--                           typenames=typenames s, extra = void}
