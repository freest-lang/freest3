{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}
module FreeST
  ( main
  , checkAndRun
  , isDev
--  , elabToTyping
  )
where

import           Elaboration.Elaboration -- ( elaboration )
import qualified Elaboration.Phase as EP
import qualified Inference.Phase as IP
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser ( parseProgram, parseAndImport )
import           Parse.Phase
import           PatternMatch.PatternMatch
import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.MkName
import           Syntax.Program (noConstructors)
import           Typing.Phase
import           Typing.Rename ( renameProgram )
import           Typing.Typing ( typeCheck )
import           Util.CmdLine
import           Util.Error
import           Util.State
import           Util.Warning

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
  s2 <- parseAndImport s1{extra = (extra s1){runOpts}}
  when (hasErrors s2) (die $ getErrors runOpts s2)

  -- print $ mVariables $ extra s2
  -- print $ pkVariables $ extra s2

  -- | PatternMatch
  let patternS = patternMatch s2
  when (hasErrors patternS) (die $ getErrors runOpts patternS)

  -- | Elaboration
  let (defs, elabS) = elaboration (pkVariables $ extra s2) (mVariables $ extra s2) patternS
  when (hasErrors elabS) (die $ getErrors runOpts elabS)

  -- | Kind Inference
  
  let infS = execState (renameProgram >> infer) (elabToInf defs elabS) --  (pkVariables $ extra elabS) (mVariables $ extra elabS)
  when (hasErrors infS) (die $ getErrors runOpts infS)
--  let (s,infS) = runState infer (elabToInf (pkVariables $ extra s2) (mVariables $ extra s2) defs elabS)

--   let var = mkVar defaultSpan "pushE"
  
-- --  print $ (types $ ast elabS) Map.! var
--   -- print $ (signatures $ ast elabS) Map.! var
--   print $ (signatures $ ast infS) Map.! var
--   print $ (definitions $ ast infS) Map.! var
  -- print $ IP.constraints $ extra infS
  -- print $ IP.mVariables $ extra infS
  -- print $ IP.pkVariables $ extra infS
--   print $ s
  -- elabToInference 
  

  -- print $ types $ ast infS
  -- print $ signatures $ ast infS
  -- print $ definitions $ ast infS

  -- | Rename & TypeCheck
  let s4 = execState typeCheck (infToTyping runOpts infS)
  when (not (quietmode runOpts) && hasWarnings s4) (putStrLn $ getWarnings runOpts s4)
  when (hasErrors s4) (die $ getErrors runOpts s4)
  
  -- | Check whether a given function signature has a corresponding
  --   binding
  let sigs = Map.keysSet (noConstructors (types $ ast s4) (signatures $ ast s4))
  let p = Map.keysSet (definitions $ ast s4)
  let bs1 = Set.difference (Set.difference sigs p) bs -- (builtins s4)
  unless (Set.null bs1) $
    die $ getErrors runOpts $ initialS {errors = Set.foldr (noSig (getSignaturesS s4)) [] bs1}
  
  -- | Check if main was left undefined, eval and print result otherwise
  let m = getMain runOpts  
  when (m `Map.member` getSignaturesS s4) $ evalAndPrint m s4 $
    forkHandlers 
      [ ("__runStdout", "__stdout")
      , ("__runStderr", "__stderr")
      , ("__runStdin", "__stdin")] 
      (getDefsS s4 Map.! m)

  where
    noSig :: Signatures -> Variable -> Errors -> Errors
    noSig sigs f acc = SignatureLacksBinding (getSpan f) f (sigs Map.! f) : acc
      
    preludeHasErrors :: FilePath -> ParseS -> ParseS -> ParseS
    preludeHasErrors f s0 s1
      | hasErrors s1 = error $ show $ errors s1
          -- s0 { warnings = NoPrelude f : warnings s0 }
      | otherwise    = s1

    forkHandlers :: [(String, String)] -> E.Exp -> E.Exp
    forkHandlers [] e = e
    forkHandlers ((fun, var) : xs) e =
      E.UnLet s (mkWild s)
        (E.App s (E.Var s (mkFork s)) (E.App s (E.Var s (mkVar s fun)) (E.Var s (mkVar s var)))) 
        $ forkHandlers xs e 
      where
        s = defaultSpan

-- Change to FromInf
-- elabToTyping :: RunOpts -> Typing.Phase.Defs -> ElabS -> TypingS
-- elabToTyping runOpts defs s = s {ast=newAst, extra = runOpts}
--   where newAst = AST {types=types $ ast s, signatures=signatures $ ast s, definitions = defs}


elabToInf :: Typing.Phase.Defs -> EP.ElabS -> IP.InferenceS
elabToInf defs s =  s {ast=newAst, extra = newExtra}
  where newAst = AST {types=types $ ast s, signatures=signatures $ ast s, definitions = defs}
        newExtra = IP.Extra {IP.mVariables= EP.mVariables (extra s),
                             IP.pkVariables=EP.pkVariables (extra s),
                             IP.constraints=[]}

        

infToTyping :: RunOpts -> IP.InferenceS -> TypingS
infToTyping runOpts s = s {ast=newAst, extra = runOpts}
  where newAst = AST {types=types $ ast s,
                      signatures=signatures $ ast s,
                      definitions = definitions $ ast s}
