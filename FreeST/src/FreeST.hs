{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}
module FreeST
  ( main
  , checkAndRun
  , isDev
  , elabToTyping
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
import           Util.State
import           Util.Warning
import           Typing.Phase
import           Typing.Rename ( renameProgram )
import           Typing.Typing ( typeCheck )
import           PatternMatch.PatternMatch

import           Control.Monad.State hiding (void)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Paths_FreeST ( getDataFileName )
import           System.Exit ( die )
import Util.State (prependEOs)

isDev :: Bool
isDev = False

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
  s2 <- (`prependEOs` s1) <$> parseAndImport s1{extra = (extra s1){runOpts}}
  when (hasErrors s2) (die $ getErrors runOpts s2)
  -- | PatternMatch
  let patternS = patternMatch s2
  when (hasErrors patternS) (die $ getErrors runOpts patternS)

  -- | Elaboration
  let (defs, elabS) = elaboration patternS
  when (hasErrors elabS) (die $ getErrors runOpts elabS)

  -- | Rename & TypeCheck
  let s4 = execState (renameProgram >> typeCheck) (elabToTyping runOpts defs elabS)
  when (not (quietmode runOpts) && hasWarnings s4) (putStrLn $ getWarnings runOpts s4)
  when (hasErrors s4) (die $ getErrors runOpts s4)
  
  -- | Check whether a given function signature has a corresponding
  --   binding
  let sigs = Map.keysSet (noConstructors (types $ ast s4) (signatures $ ast s4))
  let p = Map.keysSet (definitions $ ast s4)
  let bs1 = Set.difference (Set.difference sigs p) bs -- (builtins s4)
  unless (Set.null bs1) $
    die $ getErrors runOpts $ initialS {errors = Set.foldr (noSig (getSignaturesS s4)) [] bs1}

  evalAndPrint (getMain runOpts) s4 
  where
    noSig :: Signatures -> Variable -> Errors -> Errors
    noSig sigs f acc = SignatureLacksBinding (getSpan f) f (sigs Map.! f) : acc
      
    preludeHasErrors :: FilePath -> ParseS -> ParseS -> ParseS
    preludeHasErrors f s0 s1
      | hasErrors s1 = s0 { warnings = NoPrelude f : warnings s0 }
      | otherwise    = s1

elabToTyping :: RunOpts -> Typing.Phase.Defs -> ElabS -> TypingS
elabToTyping runOpts defs s = s {ast=newAst, extra = runOpts}
  where newAst = AST { types=types $ ast s
                     , signatures=signatures $ ast s
                     , definitions = defs
                     , evalOrder = evalOrder $ ast s
                     }
