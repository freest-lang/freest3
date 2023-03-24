{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Elaboration.TestInference where


import           Control.Monad.State.Lazy hiding (join)
import           Data.Bifunctor as Bifunctor
import           Data.Functor
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Data.Traversable (traverse)
import qualified Data.Traversable as Traversable
import           Debug.Trace
import           Elaboration.KindInference
import           Parse.ParseUtils
import           Parse.Read
import           Paths_FreeST ( getDataFileName )
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.MkName
import           Syntax.Program
import qualified Syntax.Type as T
import           System.IO
import           Util.FreestState -- (tMapM_,tMapM, initialState, FreestS(..), RunOpts(..))
import qualified Validation.Extract as Extract
import           Validation.Rename hiding (subs)
import qualified Validation.Subkind as SK
import           Validation.Substitution
import           Validation.Typing
--import           Util.CmdLine
import           Parse.Parser ( parseProgram, parseAndImport )
import           Elaboration.Elaboration
import           Data.Maybe
import           Data.List
import           Elaboration.InfState



-- TESTING

-- TESTS

test :: String -> T.Type -> E.Exp -> IO Bool
test funName' funType funBody = do
  -- rename
  let funName = mkVar defaultSpan funName'
  -- let funType = renameType funType'
  -- let funBody = renameExp funBody'
  
  -- setup
  preludeFp <- getDataFileName "Prelude.fst"
  let s0 = initialState {runOpts=defaultOpts{runFilePath=preludeFp}}
  when (hasErrors s0) (error $ show $ errors s0)
  s1 <- parseProgram s0
  -- | Solve type declarations and dualof operators
  let s1' = s1 { varEnv = Map.insert funName funType (varEnv s1) 
               , parseEnv = Map.insert funName ([],funBody) (parseEnv s1) 
               , prog = Map.insert funName funBody (prog s1) 
               }
  let s2 = execState (elaboration >> renameState) s1'
  when (hasErrors s2) (error "has errors")
  let kEnv = Map.map fst (typeEnv s2)
      -- testing
  let e' = cleanAnn 1000 (prog s2 Map.! funName)
  let ((t, u), !s) = runState (infGen kEnv e') (initial{vEnv = varEnv s2})
  let (r@(!r1,!r2),!s1) = runState (infer funName) s
  let final = subsOnKE e' (fst r)
  
  putStrLn $ "\nkConstraints: " ++ show (foldl cleanConstK [] $ kConstraints s) ++
             "\nmConstraints: " ++ show (foldl cleanConstM [] $ mConstraints s) ++ "\n"
  print r1
  print r2
  putStrLn $ "Original:\n" ++ show funBody ++ "\nAltered:\n" ++ show final ++ "\n"
  print $ show funBody == show final
  return $ show funBody == show final
  where
    userDefined m1 m2 = Map.difference m2 m1

-- runProgram :: FilePath -> IO Int
runProgram :: FilePath -> IO ()
runProgram path = do
  preludeFp <- getDataFileName "Prelude.fst"
  let s0 = initialState {runOpts=defaultOpts{runFilePath=preludeFp}}
  when (hasErrors s0) (error $ show $ errors s0)
  s1 <- parseProgram s0
  let venv = Map.keysSet (noConstructors (typeEnv s1) (varEnv s1))
  let penv = Map.keysSet (parseEnv s1)
  let bs = Set.difference venv penv
  s2 <- parseAndImport s1{builtins=bs, runOpts=(runOpts s1){runFilePath=path}}
  when (hasErrors s2) (error $ show $ errors s2)
  let kEnv = Map.map fst (typeEnv s2)
  let f x = Map.notMember x (varEnv s1)

  let s3 = execState ( fixConsTypes >> (checkNumArgs =<< getPEnvPat) >> (checkChanVar =<< getPEnvPat) >>
                       (getPEnvPat >>= setPEnvPat . addMissingVars) >> ((matchFuns =<< getPEnvPat) >>= setPEnv) >>
                       solveEquations >> ((resolve =<< getTEnv) >>= setTEnv) >> (elabVEnv =<< getVEnv) >>
                       (elabPEnv =<< getPEnv) >> ((resolve =<< getVEnv) >>= setVEnv) >> ((resolve =<< getPEnv) >>= setPEnv)
                     ) s2

  -- this one is only to get the initial types & expressions 
  let si =  emptyPEnv $ execState (buildProg >> renameState) s3
  
  let ((vEnv, pEnv), state) = runState (do
       m1 <- tMapWithKeyM (\k t -> if f k then cleanType (typeEnv s3) t else return t) (varEnv s3)
       m2 <- tMapWithKeyM (\k (xs,e) -> if f k then (xs,) <$> cleanExp (typeEnv s3) e else return (xs,e)) (parseEnv s3)
       return (m1, m2)) initial
        
  let s4 = emptyPEnv $ execState (buildProg >> renameState) (s3{varEnv = vEnv, parseEnv = pEnv})

  let (sk,stt) = runState (collectAll kEnv (Map.filterWithKey (\k _ -> f k) (prog s4)) >> infer (mkVar defaultSpan "")) (state{vEnv = varEnv s4})
  
  !s <- foldM (\acc (k, e) -> do
          let initialE = prog s4 Map.! k
          let finalE = subsOnKE e (fst sk)
          let initialT = varEnv s4 Map.! k
          let finalT = subsOnKT (typeOnType (typeFromExp (varEnv s4) finalE) initialT) (fst sk)
          let iT = varEnv si Map.! k 
          let iE = prog si Map.! k

          if show iT /= show finalT || show iE /= show finalE
            then return $ (k,iT,iE, finalT, finalE) : acc
            else return acc
          
        ) [] (Map.toList $ Map.filterWithKey (\k _ -> f k) (prog s4))

  putStrLn path
  !s <- mapM_ (\(k,it,ie,ft,fe) -> -- putStrLn (show k ++ "\n") >>
                putStrLn (show k ++ " : " ++ show it ++ "\n" ++
                          show k ++ " = " ++ show ie ++ "\n\n" ++
                          show k ++ " : " ++ show ft ++ "\n" ++
                          show k ++ " = " ++ show fe ++ "\n\n"                          
                         ) >> putStrLn "--------------------\n"
              ) s           
  return ()
--  return $ length $ kVariables stt


collectAll :: K.KindEnv -> Prog -> InferState ()
collectAll kEnv = foldM_ (\acc k -> infGen kEnv k >>= \x -> pure (x:acc)) []
  -- do
  -- !s <- foldM (\acc k -> infGen kEnv k >>= \x -> pure (x:acc)) [] prog -- (Map.toList prog)
  -- return s
  
runProgs :: IO ()
runProgs = do
  let prepend = "/Users/balmeida/workspaces/ContextFreeSession/FreeST/test/Programs/ValidTests"
--  let prepend = "/tmp"
  let progs = ["/Patterns/dupFunDecl/dupFunDecl.fst"
              , "/Patterns/nestedFunPatterns/nestedFunPatterns.fst"
              , "/Patterns/dupCases/dupCases.fst"
              , "/Patterns/multipleDecls/multipleDecls.fst"
              , "/Patterns/intListSize/intListSize.fst"
              , "/Patterns/intList/intList.fst"
              , "/Patterns/sessionsWithinData/sessionsWithinData.fst"
              , "/Patterns/intListElem/intListElem.fst"
              , "/Patterns/nestedCasePatterns/nestedCasePatterns.fst"
              , "/Patterns/perceptron/perceptron.fst"
              , "/Patterns/listEquality/listEquality.fst"
              , "/Patterns/dollarSumFilterMap/dollarSumFilterMap.fst"
              , "/Patterns/intListSum/intListSum.fst"
              , "/Patterns/multipleListTransformations/multipleListTransformations.fst"
              
              , "/Lists/ListScans/ListScans.fst"
              , "/Lists/ListSearch/ListSearch.fst"
              , "/Lists/ListIndexing/ListIndexing.fst"
              , "/Lists/ListFolds/ListFolds.fst"
              , "/Lists/ListSublists/ListSublists.fst"
              , "/Lists/ListBasics/ListBasics.fst"
              , "/Lists/ListTransformations/ListTransformations.fst"
              
              , "/HigherOrder/HOClosure/HOClosure.fst"
              , "/HigherOrder/HOTree/HOTree.fst"
              , "/HigherOrder/HOInput/HOInput.fst"
              , "/HigherOrder/HOFun/HOFun.fst"
              , "/Higherorder/HOVar/HOVar.fst"
              , "/HigherOrder/HOSendForall/HOSendForall.fst"
              , "/HigherOrder/sendClosure/sendClosure.fst"
              , "/HigherOrder/HOForall/HOForall.fst" -- TODO: check 
              ,  "/HigherOrder/HOOmega/HOOmega.fst"
              , "/HigherOrder/HOPair/HOPair.fst"
              , "/HigherOrder/HOData/HOData.fst"
              
              , "/Applications/genetic/genetic.fst"
              , "/Applications/json/json.fst"
              , "/Applications/tabuada/tabuada.fst"
              , "/Applications/tech-store/tech-store.fst" 
              , "/Applications/movingAverage/movingAverage.fst"
              , "/Applications/ordering/ordering.fst"
              , "/Applications/perceptron/perceptron.fst"
              , "/Applications/Sorting/Sorting.fst"
              , "/Applications/OddEvenSort/OddEvenSort.fst"
              
              , "/SharedChannels/cake-store/cake-store.fst"  -- TODO: not in delta check 
              , "/SharedChannels/ftpServer/ftpServer.fst"
              , "/SharedChannels/sharedBag/sharedBag.fst"
              , "/SharedChannels/shared-queue/shared-queue.fst"
              , "/SharedChannels/random/random.fst"
              , "/SharedChannels/sharedBool/sharedBool.fst"
              , "/SharedChannels/shared-math-server/shared-math-server.fst" -- TODO: not in delta check
              , "/SharedChannels/sync-channel/sync-channel.fst"
              
              , "/SystemF/SystemFPairs/SystemFPairs.fst" -- differs
              , "/SystemF/SystemFNats/SystemFNats.fst"  -- Have to adapt all the TypeAbs to 1  -- TODO: check 
              , "/SystemF/SystemFTrees/SystemFTrees.fst"
              , "/SystemF/SystemFBooleans/SystemFBooleans.fst"
              , "/SystemF/SystemFLists/SystemFLists.fst" -- TODO: check 
              ,"/SystemF/SystemFSums/SystemFSums.fst" -- TODO: check 
              , "/SystemF/SystemFWarmUps/SystemFWarmUps.fst"
              , "/SystemF/SystemFBins/SystemFBins.fst"
              
              , "/PatternsChannel/regularTreeStreamAlt/regularTreeStreamAlt.fst"
              , "/PatternsChannel/arithExprServerRegular1/arithExprServerRegular1.fst"
              , "/PatternsChannel/genetic/genetic.fst"
              , "/PatternsChannel/ListRW/ListRW.fst"
              , "/PatternsChannel/nTreeSend/nTreeSend.fst"
              , "/PatternsChannel/remoteListTransform/remoteListTransform.fst"
              , "/PatternsChannel/sendListType/sendListType.fst"
              , "/PatternsChannel/regularTreeStream/regularTreeStream.fst" -- TODO: check 
              , "/PatternsChannel/channelMathServer/channelMathServer.fst"
              , "/PatternsChannel/json/json.fst"
              , "/PatternsChannel/tabuada/tabuada.fst"
              , "/PatternsChannel/ordering/ordering.fst"
              , "/PatternsChannel/lazyTreeTraversal/lazyTreeTraversal.fst"
              , "/PatternsChannel/helloWorldList/helloWorldList.fst" -- TODO: check 
              , "/PatternsChannel/sendList1/sendList1.fst"
              , "/PatternsChannel/sendTree/sendTree.fst"
              , "/PatternsChannel/Sorting/Sorting.fst"
              , "/PatternsChannel/TreeTransform/TreeTransform.fst"
              , "/PatternsChannel/sendTreeType/sendTreeType.fst"
              , "/PatternsChannel/listSend/listSend.fst"
              
              , "/Modules/LongCycle/LongCycle.fst"
              , "/Modules/DirInDir/DirInDir.fst"
              , "/Modules/Cycle/Cycle.fst"
              , "/Modules/Simple/Simple.fst"
              , "/Modules/Self/Self.fst"
              , "/Modules/BackImport/BackImport.fst"
              
              , "/Functional/lambdaHO/lambdaHO.fst"
              , "/Functional/charBin/charBin.fst"
              , "/Functional/isPrime/isPrime.fst"
             , "/Functional/fstSnd/fstSnd.fst"
              , "/Functional/collect/collect.fst"
              , "/Functional/eqDataAndCons/eqDataAndCons.fst"
              , "/Functional/ioFunAsPureArg/ioFunAsPureArg.fst"
              , "/Functional/boolEq/boolEq.fst"
              , "/Functional/TuplesAsPairsOfPairs/TuplesAsPairsOfPairs.fst"
              , "/Functional/dot/dot.fst"                                              -- DIFFERS
              , "/Functional/polySUSL/polySUSL.fst"
              , "/Functional/multDivPrecedence/multDivPrecedence.fst"
              , "/Functional/UnArrowAsLin/UnArrowAsLin.fst"
              , "/Functional/dataDiffNumBoundVars/dataDiffNumBoundVars.fst"
              , "/Functional/charEquality/charEquality.fst"
              , "/Functional/arrowWithDataWithinType/arrowWithDataWithinType.fst"
             , "/Functional/ListRW/ListRW.fst"
              , "/Functional/intListSize/intListSize.fst"
              , "/Functional/ImplicitForall/ImplicitForall.fst" -- TODO: check
              , "/Functional/processes/processes.fst" -- TODO: TEST THIS ONE FOR THE NEW APPROACH (BUILD TYPE)
              , "/Functional/n-tuples/n-tuples.fst"
              , "/Functional/recData/recData.fst"
              , "/Functional/polySUTL/polySUTL.fst"
              , "/Functional/intList/intList.fst"
              , "/Functional/multPrecedence/multPrecedence.fst"
              , "/Functional/consAsFun/consAsFun.fst"
              , "/Functional/reversedFunApp/reversedFunApp.fst"
              , "/Functional/diffEqTypeNames/diffEqTypeNames.fst"
              , "/Functional/recPair/recPair.fst" -- TODO: BUILD TYPE
              ,  "/Functional/dataUn/dataUn.fst"
              , "/Functional/scope/scope.fst"             
              , "/Functional/arrowLinType/arrowLinType.fst"
              , "/Functional/BlockCommentInSig/BlockCommentInSig.fst"
              , "/Functional/fEqg/fEqg.fst"
              , "/Functional/multipleOps/multipleOps.fst"
              , "/Functional/typeDecl/typeDecl.fst"
              , "/Functional/skipSkipSL/skipSkipSL.fst"
              , "/Functional/TAppAssoc/TAppAssoc.fst"
              , "/Functional/intListElem/intListElem.fst"
              , "/Functional/recFun/recFun.fst"
              , "/Functional/simpleApp/simpleApp.fst"
              , "/Functional/semicolonPriority/semicolonPriority.fst"
              , "/Functional/funAsArg/funAsArg.fst"
              , "/Functional/arrowType/arrowType.fst"
              , "/Functional/parensPrecedence/parensPrecedence.fst"
              , "/Functional/multiVarTypeSig/multiVarTypeSig.fst"
              , "/Functional/fact/fact.fst"
              , "/Functional/fixPointAlt/fixPointAlt.fst"                          -- DIFFERS
              , "/Functional/doubleUnderscore/doubleUnderscore.fst"
              , "/Functional/listEquality/listEquality.fst"
              ,  "/Functional/hungry/hungry.fst"
              , "/Functional/multipleParens/multipleParens.fst"
              , "/Functional/dollarSumFilterMap/dollarSumFilterMap.fst"
              , "/Functional/fact10/fact10.fst"
              , "/Functional/simpleVar/simpleVar.fst"
              , "/Functional/mutualRecData/mutualRecData.fst"
              , "/Functional/polyTwoParams/polyTwoParams.fst"
              , "/Functional/fixZcombinator/fixZcombinator.fst"                    -- DIFFERS
              , "/Functional/triple/triple.fst"
              , "/Functional/average/average.fst"
              , "/Functional/mainMissing/mainMissing.fst"
              , "/Functional/intListSum/intListSum.fst"
              , "/Functional/TypeVarNotShadowed/TypeVarNotShadowed.fst"
              , "/Functional/multipleListTransformations/multipleListTransformations.fst" 
              
              , "/Guards/ex_08-6b/ex_08-6b.fst"
              , "/Guards/ex_01-11a/ex_01-11a.fst"
              , "/Guards/isPrime/isPrime.fst"
              , "/Guards/ex_07-14/ex_07-14.fst"
              , "/Guards/ex_07-6/ex_07-6.fst"
              , "/Guards/ex_07-8/ex_07-8.fst"
              , "/Guards/boolEq/boolEq.fst"
              , "/Guards/ex_01-3/ex_01-3.fst"
              , "/Guards/ex_01-2/ex_01-2.fst"
              , "/Guards/ex_05-19/ex_05-19.fst"
              , "/Guards/ex_05-21/ex_05-21.fst"
              , "/Guards/ex_01-10/ex_01-10.fst"
              , "/Guards/ex_05-18/ex_05-18.fst"
              , "/Guards/ex_04-1i/ex_04-1i.fst"
              , "/Guards/ex_01-11b/ex_01-11b.fst"
              , "/Guards/intListElem/intListElem.fst"
              , "/Guards/ex_01-9/ex_01-9.fst"
              , "/Guards/ex_03-4/ex_03-4.fst"
              , "/Guards/fact/fact.fst"
              , "/Guards/ex_01-1b/ex_01-1b.fst"
              , "/Guards/ex_01-12/ex_01-12.fst"
              , "/Guards/dollarSumFilterMap/dollarSumFilterMap.fst"
              , "/Guards/ex_05-8c/ex_05-8c.fst"
              , "/Guards/ex_05-16e/ex_05-16e.fst"
              , "/Guards/ex_01-14/ex_01-14.fst"
              , "/Guards/ex_04-1b/ex_04-1b.fst"
              , "/Guards/multipleListTransformations/multipleListTransformations.fst"
          
              , "/SessionTypes/crisscross/crisscross.fst"
              , "/SessionTypes/pingPongDiverge/pingPongDiverge.fst"
              , "/Sessiontypes/regularTreeStreamAlt/regularTreeStreamAlt.fst"
              , "/SessionTypes/chainOfChannelOps/chainOfChannelOps.fst"
              , "/SessionTypes/pipeline/pipeline.fst"
              , "/SessionTypes/sendAsSendArg/sendAsSendArg.fst"
              , "/SessionTypes/Unnormed/Unnormed.fst"
              , "/SessionTypes/sendList/sendList.fst"
              , "/SessionTypes/arithExprServerRegular1/arithExprServerRegular1.fst"
              , "/SessionTypes/sendReceive/sendReceive.fst"
              , "/SessionTypes/boolServer2/boolServer2.fst"
              , "/SessionTypes/boolServer3/boolServer3.fst"
              , "/SessionTypes/partialFork/partialFork.fst"
              , "/SessionTypes/arithExprServerRegular/arithExprServerRegular.fst"
              , "/SessionTypes/boolServerRecursive/boolServerRecursive.fst"
              , "/SessionTypes/nTreeSend/nTreeSend.fst"
              , "/SessionTypes/streams/streams.fst"
              , "/SessionTypes/remoteListTransform/remoteListTransform.fst"
              , "/SessionTypes/FiniteStreamEnd/FiniteStreamEnd.fst"
              , "/SessionTypes/sendTreeRegular/sendTreeRegular.fst"
              , "/SessionTypes/dualMessage/dualMessage.fst"
              , "/SessionTypes/sessionsWithinData/sessionsWithinData.fst"
              , "/SessionTypes/sendReceive1/sendReceive1.fst"
              , "/SessionTypes/sendListType/sendListType.fst"
              , "/SessionTypes/typeNameInSig/typeNameInSig.fst"
              , "/SessionTypes/usend/usend.fst" -- TODO: check (closure definition)
              , "/SessionTypes/anbn/anbn.fst"
              , "/SessionTypes/math/math.fst"
              , "/SessionTypes/math/mathplus/math.fst"
              , "/SessionTypes/partialReceive/partialReceive.fst"
              , "/SessionTypes/closeSkipEnd/closeSkipEnd.fst"
              , "/SessionTypes/StackRecClient/StackRecClient.fst"
              , "/SessionTypes/regularTreeStream/regularTreeStream.fst"
              , "/SessionTypes/deadlock/deadlock.fst"
              , "/SessionTypes/typedefEnd/typedefEnd.fst"
              , "/SessionTypes/recChan/recChan.fst"
              , "/SessionTypes/arithExprServer/arithExprServer.fst"
              , "/SessionTypes/SkipSkip/SkipSkip.fst"
              , "/SessionTypes/boolServer1/boolServer1.fst"
              , "/SessionTypes/lazyTreeTraversal/lazyTreeTraversal.fst"
              , "/SessionTypes/sendInt/sendInt.fst"
              , "/SessionTypes/helloWorldList/helloWorldList.fst"
              , "/SessionTypes/math1/math1.fst"
              , "/SessionTypes/sendTree/sendTree.fst"
              , "/SessionTypes/typeTeqU/typeTeqU.fst"
              , "/SessionTypes/TreeTransform/TreeTransform.fst"
              , "/SessionTypes/dupConsChan/dupConsChan.fst"
              , "/SessionTypes/sendTreeType/sendTreeType.fst"
              , "/SessionTypes/listSend/listSend.fst"
              , "/SessionTypes/infiniteChannels/infiniteChannels.fst"
              , "/SessionTypes/Stack/Stack.fst"
              , "/SessionTypes/sendRecvSameThread/sendRecvSameThread.fst"
              , "/SessionTypes/ImpredicativeSend/ImpredicativeSend.fst"
              , "/SessionTypes/boolServer/boolServer.fst"
              , "/SessionTypes/DyckWords/DyckWords.fst" -- TODO: check loop
              , "/SessionTypes/partialSend/partialSend.fst"
              , "/SessionTypes/ndual/ndual.fst"
              , "/SessionTypes/ndualRec/ndualRec.fst"
              ]

  !s <- mapM (runProgram . (prepend ++)) progs
--  !s <- foldM (\acc t -> runProgram (prepend ++ t) >>= \i -> pure $ acc + i ) 0 progs
--  print s
  return ()

runTests :: IO ()
runTests = do
  -- TODO: parallel, forkWith: depend on constants (check)  
  let tests = [("fst'", fstT, fstE)
              -- ,("snd'",sndT,sndE)
              -- ,("id'",idT,idE)
              -- ,("flip'",flipT,flipE)
              -- ,("until'",untilT,untilE)
              -- ,("curry'",curryT,curryE)
              -- ,("uncurry'",uncurryT,uncurryE)
              -- ,("swap'",swapT,swapE)
              -- ,("fix'",fixT,fixE)
              -- ,("sink'",sinkT,sinkE)
              -- ,("repeat'",repeatT,repeatE)              
              -- ,("parallel'",parallelT,parallelE)
              -- ,("consume'",consumeT,consumeE)
              -- ,("receiveAndClose'",receiveAndCloseT,receiveAndCloseE)
              -- ,("receive_'",receiveT,receiveE)
              -- ,("send_'",sendT,sendE)
              -- , ("accept'",acceptT,acceptE)
              -- ,("forkWith'",forkWithT,forkWithE)
              -- ,("runServer'",runServerT,runServerE)
              -- ,("__hGenericPut'",__hGenericPutT,__hGenericPutE)
              -- ,("hPrint'",hPrintT,hPrintE)
              -- ,("hPrint_'",hPrint_T,hPrint_E)
              -- ,("__hGenericGet'",__hGenericGetT,__hGenericGetE)
              -- ,("print'",printT,printE)
              -- ,("__hGenericGet_'",__hGenericGet_T,__hGenericGet_E)
              -- ,("iFold",iFoldT,iFoldE)
              -- ,("'",T,E)
              ]
  
  bs <- mapM (\(a,b,c) -> putStr (a ++ " = ") >>
             test a b c
        ) tests
  let xs = filter id bs
  putStrLn $ "\nPassed " ++ show (length xs) ++ " of " ++ show (length bs) ++ " tests!"

-- TESTS

fstT = read "forall a:1T . forall b:*T . (a,b) -> a" :: T.Type
fstE = read "Λa:1T b:*T => λp:(a,b) -> let (x,_) = p in x" :: E.Exp
sndT = read "forall a:*T b:1T . (a, b) -> b" :: T.Type
sndE = read "Λa:*T b:1T => λp:(a,b) -> let (_,x) = p in x" :: E.Exp
idT = read "forall a:1T . a -> a" :: T.Type -- * on prelude
idE = read "Λa:1T => λx:a -> x" :: E.Exp
flipT = read "forall a:1T b:*T c:1T . (a -> b -> c) -> b -> a -> c" :: T.Type -- *, *, * on prelude
flipE = read "Λa:1T b:*T c:1T => λf:(a -> b -> c) x:b y:a ->  f y x" :: E.Exp
untilT = read "forall a:*T . (a -> Bool) -> (a -> a) -> a -> a" :: T.Type
untilE = read "Λa:*T => λp:(a -> Bool) f:(a -> a) x:a ->  if p x then x else until' @a p f (f x)" :: E.Exp
curryT = read "forall a:*T b:1T c:1T . ((a, b) -> c) -> a -> b -> c" :: T.Type -- was: * * *
curryE = read "Λa:*T b:1T c:1T => λf:((a, b) -> c) x:a y:b -> f (x, y)" :: E.Exp
uncurryT = read "forall a:*T b:*T c:1T . (a -> b -> c) -> (a, b) -> c" :: T.Type -- was: * * *
uncurryE = read "Λa:*T b:*T c:1T => λf:(a -> b -> c) p:(a, b) -> f (fst@a @b p) (snd @a @b p)" :: E.Exp
swapT = read "forall a:1T b:1T . (a, b) -> (b, a)" :: T.Type -- was * *
swapE = read "Λa:1T b:1T => λx:(a, b) -> let (a, b) = x in (b, a)" :: E.Exp
fixT = read "forall a:1T . ((a -> a) -> (a -> a)) -> (a -> a)" :: T.Type -- was: *
fixE = read $ "Λa:1T => λf:((a -> a) -> (a -> a)) -> " ++ 
              "(λx:(μb.b -> (a -> a)) -> f (λz:a -> x x z))" ++
              "(λx:(μb.b -> (a -> a)) -> f (λz:a -> x x z))" :: E.Exp
sinkT = read "forall a:*T . a -> ()" :: T.Type 
sinkE = read "Λa:*T => λ_:a -> ()" :: E.Exp 
repeatT = read "forall a:*T . Int -> (() -> a) -> ()" :: T.Type
repeatE = read $ "Λa:*T => λn:Int thunk:(() -> a) -> " ++
 "    if n <= 0 " ++
 "    then () " ++
 "    else " ++
 "        thunk (); " ++
 "        repeat @a (n - 1) thunk " :: E.Exp

parallelT = read "forall a:*T . Int -> (() -> a) -> ()" :: T.Type -- TODO: wrong
parallelE = read "Λa:*T => λn:Int thunk:(() -> a) -> repeat @() n (λ_:() -> fork @a thunk)" :: E.Exp
consumeT = read "forall a:1T b:1S . (a -> ()) -> ?a;b 1-> b" :: T.Type -- Was *T 1S
consumeE = read "Λa:1T b:1S => λf:(a -> ()) ch:?a;b -> let (x, ch) = receive ch in f x; ch" :: E.Exp
receiveAndCloseT = read "forall a:1T . ?a;End -> a " :: T.Type
receiveAndCloseE = read "Λa:1T => λc:?a;End -> let (x, c) = receive c in close c; x" :: E.Exp
receiveT = read "forall a:1T . *?a -> a" :: T.Type
receiveE = read "Λa:1T => λch:*?a -> ch |> receive |> fst @a @*?a" :: E.Exp
sendT = read "forall a:1T . a -> *!a 1-> ()" :: T.Type
sendE = read "Λa:1T => λx:a -> λch:*!a 1-> ch |> send x |> sink @*!a" :: E.Exp
acceptT = read "forall a:1S . *!a -> dualof a" :: T.Type
acceptE = read "Λa:1S => λch:*!a -> let (x, y) = new @a () in send x ch; y" :: E.Exp
forkWithT = read "forall a:1S b . (dualof a 1-> b) -> a" :: T.Type
forkWithE = read $ "Λa:1S b:*T => λf:(dualof a 1-> b) -> " ++
                 "let (x, y) = new @a () in fork (λ_:() 1-> f y); x" :: E.Exp
runServerT = read "forall a:1S b:*T . (b -> dualof a 1-> b) -> b -> *!a -> Diverge" :: T.Type
runServerE = read $ "Λa:1S b:*T => λ handle:(b -> dualof a 1-> b) state:b ch:*!a -> " ++  
                    "runServer @a @b handle (handle state (accept @a ch)) ch" :: E.Exp
__hGenericPutT = read "forall a:*T . (OutStream -> !a;OutStream) -> a -> OutStream -> OutStream" :: T.Type
__hGenericPutE = read "Λa:*T => λsel:(OutStream -> !a;OutStream) x:a outStream:OutStream -> sel outStream |> send x" :: E.Exp
hPrintT = read "forall a:*T . a -> OutStream -> OutStream" :: T.Type
hPrintE = read "Λa:*T => λx:a -> hPutStrLn (show @a x)" :: E.Exp

hPrint_T = read "forall a:*T . a -> OutStreamProvider -> ()" :: T.Type
hPrint_E = read "Λa:*T => λx:a c:OutStreamProvider -> __hGenericPut_ @a (hPrint @a) x c" :: E.Exp

__hGenericGetT = read "forall a:1T . (InStream -> ?a;InStream) -> InStream -> (a, InStream)" :: T.Type
__hGenericGetE = read "Λa:1T => λsel:(InStream -> ?a;InStream) ch:InStream -> receive $ sel ch" :: E.Exp -- was: *
printT = read "forall a:*T . a -> ()" :: T.Type
printE = read "Λa:*T => λx:a -> putStrLn $ show @a x" :: E.Exp
__hGenericGet_T = read "forall a:1T . (InStream -> (a, InStream)) -> InStreamProvider -> a" :: T.Type
__hGenericGet_E = read  $ "Λa:1T => λgetF:(InStream -> (a, InStream)) inp:InStreamProvider -> " ++ 
  "let (x, ch) = getF $ receive_ @InStream inp in "++
  "let _ = hCloseIn ch in x" :: E.Exp
__hGenericPut_T = read "forall a . (a -> OutStream -> OutStream) -> a -> OutStreamProvider -> ()" :: T.Type
__hGenericPut_E = read "Λa:*T => λputF:(a -> OutStream -> OutStream) x:a outProv:OutStreamProvider -> hCloseOut $ putF x $ receive_ @OutStream outProv" :: E.Exp

iFoldT = read "forall a: 1T b: 1S . a -> (Int -> a -> a) 1-> (rec x:1S . &{NilC: Skip, ConsC: !Int; x});b 1-> (a, b)" :: T.Type
iFoldE = read $ "Λa:1T b:1S => λn:a -> λf:(Int -> a -> a) 1-> λc:(rec x:1S . &{NilC: Skip, ConsC: !Int; x});b 1-> " ++
  "  match c with { " ++
  "    NilC c -> (n, c), " ++
  "    ConsC c -> let (m, c) = receive c in " ++ 
  "              let (n, c) = iFold  @a @b n f c in " ++ 
  "              (f m n, c) " ++ 
  "  }" :: E.Exp

-- T = read "" :: T.Type
-- E = read "" :: E.Exp

-- | Aux functions for testing


pureFKVar i = K.KindVar defaultSpan $ mkVar defaultSpan ("χ" ++ show i )

class Clean a where
  cleanAnn :: Int -> a -> a

instance Clean E.Exp where 
  cleanAnn i (E.TypeAbs p (Bind b x _ e)) =
    E.TypeAbs p (Bind b x (pureFKVar i) (cleanAnn (i+1) e))
  cleanAnn i (E.Abs p m (Bind b x t e)) =
    E.Abs p m (Bind b x t (cleanAnn i e))
  cleanAnn i (E.UnLet s x e1 e2     ) =
    E.UnLet s x (cleanAnn i e1) (cleanAnn (i+100) e2)
  cleanAnn i (E.App p e1 e2          ) =
    E.App p (cleanAnn i e1) (cleanAnn (i+100) e2)
  cleanAnn i (E.Cond p e1 e2 e3       ) =
    E.Cond p (cleanAnn i e1) (cleanAnn (i+100) e2) (cleanAnn (i+200) e2)
  cleanAnn i (E.Pair p e1 e2         ) =
    E.Pair p (cleanAnn i e1) (cleanAnn (i+100) e2)
  cleanAnn i (E.BinLet p x y e1 e2   ) =
    E.BinLet p x y (cleanAnn i e1) (cleanAnn (i+100) e2)
  cleanAnn i (E.Case  p e fm        ) =
    E.Case p (cleanAnn i e) (Map.map (Bifunctor.second (cleanAnn (i+100))) fm)
  cleanAnn i (E.TypeApp  p e t        ) =
    E.TypeApp p (cleanAnn i e) (cleanAnn (i+100) t)    
  cleanAnn _ e = e


instance Clean T.Type where
  cleanAnn i (T.Arrow p m t1 t2) = T.Arrow p m (cleanAnn i t1) (cleanAnn i t2)
  cleanAnn i (T.Labelled p s m) = T.Labelled p s (Map.map (cleanAnn i) m)
  cleanAnn i (T.Semi p t1 t2   ) = T.Semi p (cleanAnn i t1) (cleanAnn (i + 100) t2)
  cleanAnn i (T.Message p pol t) = T.Message p pol (cleanAnn i t)
  cleanAnn i (T.Forall p (Bind b x _ t)) = T.Forall p (Bind b x (pureFKVar i) (cleanAnn (i+100) t))
  cleanAnn i (T.Rec p (Bind b x k t)) = T.Rec p (Bind b x k (cleanAnn i t))
  cleanAnn i (T.Dualof p t   ) = T.Dualof p (cleanAnn i t)
  cleanAnn _ t = t


cleanExp :: TypeEnv -> E.Exp -> InferState E.Exp
cleanExp tEnv (E.TypeAbs p (Bind b x _ e)) = do
  kv <- freshKindVar p
  addKVariableFromK kv
  addBind x kv
  E.TypeAbs p . Bind b x kv <$> cleanExp tEnv e
cleanExp tEnv (E.Abs p m (Bind b x t e)) = do
  t'<- cleanType tEnv t
  E.Abs p m . Bind b x t' <$> cleanExp tEnv e
cleanExp tEnv (E.UnLet s x e1 e2) = E.UnLet s x <$> cleanExp tEnv e1 <*> cleanExp tEnv e2
cleanExp tEnv (E.App p e1 e2) = E.App p <$> cleanExp tEnv e1 <*> cleanExp tEnv e2
cleanExp tEnv (E.Cond p e1 e2 e3) = E.Cond p <$> cleanExp tEnv e1 <*> cleanExp tEnv e2 <*> cleanExp tEnv e2
cleanExp tEnv (E.Pair p e1 e2) = E.Pair p <$> cleanExp tEnv e1 <*> cleanExp tEnv e2
cleanExp tEnv (E.BinLet p x y e1 e2   ) = E.BinLet p x y <$> cleanExp tEnv e1 <*> cleanExp tEnv e2
cleanExp tEnv (E.Case  p e fm        ) =
  E.Case p <$> cleanExp tEnv e <*> mapM (\(xs,e) -> (xs,) <$> cleanExp tEnv e) fm
cleanExp tEnv (E.TypeApp  p e t        ) = E.TypeApp p <$> cleanExp tEnv e <*> cleanType tEnv t
cleanExp tEnv e = return e


cleanType :: TypeEnv -> T.Type -> InferState T.Type
cleanType tEnv (T.Arrow p m t1 t2) = T.Arrow p m <$> cleanType tEnv t1 <*> cleanType tEnv t2   
cleanType tEnv (T.Labelled p s m) = T.Labelled p s <$> mapM (cleanType tEnv) m
cleanType tEnv (T.Semi p t1 t2   ) = T.Semi p <$> cleanType tEnv t1 <*> cleanType tEnv t2
cleanType tEnv (T.Message p pol t) = T.Message p pol <$> cleanType tEnv t
cleanType tEnv fall@(T.Forall p (Bind b x _ t)) = do
  kv <- freshKindVar p
  addBind x kv
  addKVariableFromK kv
  T.Forall p . Bind b x kv <$> cleanType tEnv t
cleanType tEnv (T.Rec p (Bind s a k@(K.Kind _ K.Un K.Session) t@(T.Semi _ T.Message{} (T.Var _ b))))
  | a == b = T.Rec p . Bind s a k <$> cleanType tEnv t
cleanType tEnv (T.Rec p (Bind s a k@(K.Kind _ K.Un K.Session) (T.Labelled s' c@T.Choice{} m)))
  | all (\case {(T.Var _ b) -> a == b ; _ -> False }) m =
     T.Rec p . Bind s a k .T.Labelled s' c  <$> mapM (cleanType tEnv) m
cleanType tEnv (T.Rec p (Bind b x k t))
  | Map.member x tEnv =   T.Rec p . Bind b x k <$> cleanType tEnv t
  | otherwise         =  do
      kv <- freshKindVar p
      addKVariableFromK kv
      T.Rec p . Bind b x kv <$> cleanType tEnv t
cleanType tEnv (T.Dualof p t   ) = T.Dualof p <$> cleanType tEnv t
cleanType tEnv t = return t


subsOnKE :: E.Exp -> SubsK -> E.Exp
subsOnKE = Map.foldlWithKey subsKE

subsOnKT :: T.Type -> SubsK -> T.Type
subsOnKT = Map.foldlWithKey subsKE

-- subsOnKindsT = Map.foldlWithKey (\acc k s -> subsKT k s acc )

class SubsKE a where
  subsKE :: a -> Variable -> K.Kind -> a

instance SubsKE E.Exp where
  subsKE (E.TypeAbs p (Bind b x v e)) kv k
    | isKVar v && kv == fromKToVar v =
        E.TypeAbs p (Bind b x k (subsKE e kv k))
    | otherwise =
        E.TypeAbs p (Bind b x v (subsKE e kv k))
  subsKE (E.Abs p m (Bind b x t e)) kv k =
    E.Abs p m (Bind b x (subsKE t kv k) (subsKE e kv k))
  subsKE (E.UnLet s x e1 e2     ) kv k = 
    E.UnLet s x (subsKE e1 kv k) (subsKE e2 kv k)
  subsKE (E.App p e1 e2          ) kv k =
    E.App p (subsKE e1 kv k) (subsKE e2 kv k)
  subsKE (E.Cond p e1 e2 e3       ) kv k =
    E.Cond p  (subsKE e1 kv k) (subsKE e2 kv k) (subsKE e3 kv k)
  subsKE (E.Pair p e1 e2         ) kv k =
    E.Pair p  (subsKE e1 kv k) (subsKE e2 kv k)
  subsKE (E.BinLet p x y e1 e2   ) kv k =
    E.BinLet p x y  (subsKE e1 kv k) (subsKE e2 kv k)
  subsKE (E.Case  p e fm        ) kv k =
    E.Case  p e (Map.map (Bifunctor.second (\e -> subsKE e kv k)) fm)
  subsKE (E.TypeApp  p e t        ) kv k =
    E.TypeApp  p (subsKE e kv k) (subsKE t kv k)
  subsKE e _ _ = e


instance SubsKE T.Type where
  subsKE (T.Forall p (Bind b x v t)) kv k
    | isKVar v && kv == fromKToVar v = T.Forall p (Bind b x k (subsKE t kv k))
    | otherwise                      =
--      trace ("\nv: " ++ show v ++ "\n"++ show kv ++ "\n") $
      T.Forall p (Bind b x v (subsKE t kv k))
  subsKE (T.Arrow p m t1 t2) kv k = T.Arrow p m (subsKE t1 kv k) (subsKE t2 kv k)
  subsKE (T.Labelled p s m) kv k = T.Labelled p s (Map.map (\t -> subsKE t kv k) m)
  subsKE (T.Semi p t1 t2   ) kv k = T.Semi p (subsKE t1 kv k) (subsKE t2 kv k)
  subsKE (T.Message p pol t) kv k = T.Message p pol (subsKE t kv k)
  subsKE (T.Rec p (Bind b x v t)) kv k
    | isKVar v && kv == fromKToVar v = T.Rec p (Bind b x k (subsKE t kv k))
    | otherwise                      =     
        T.Rec p (Bind b x v (subsKE t kv k))
  subsKE (T.Dualof p t   ) kv k = T.Dualof p (subsKE t kv k)
  subsKE t _ _  = t
                              



-- subsOnType :: Prog -> E.Exp -> T.Type -> T.Type
-- subsOnType prog (E.Abs _ _ (Bind _ x t e)) (T.Arrow s m t1 t2) =
--   T.Arrow s m (typeOnType t1 t) (subsOnType prog e t2)
-- subsOnType prog (E.TypeAbs _ (Bind _ _ k e)) (T.Forall s (Bind s1 x _ t)) =
--   T.Forall s $ Bind s1 x k $ subsOnType prog e t
-- subsOnType prog (E.TypeApp _ e t) (T.Forall s (Bind s1 x _ u)) = 
--   subsOnType prog e u
-- subsOnType prog (E.App _ e1 e2) (T.Arrow s m t1 t2) = T.Arrow s m (subsOnType prog e1 t1) (subsOnType prog e2 t2)
-- subsOnType _ _ t = t





-- SUBS kinds from the fst on the snd 
typeOnType :: T.Type -> T.Type -> T.Type
typeOnType (T.Arrow _ _ t1 t2) (T.Arrow s m t3 t4) = T.Arrow s m (typeOnType t1 t3) (typeOnType t2 t4)
typeOnType (T.Forall _ (Bind _ x k t)) (T.Forall s (Bind s' y _ u)) = T.Forall s $ Bind s' y k $ typeOnType t u
--  | otherwise = error $ show x ++ " /= " ++ show y
typeOnType (T.Rec _ (Bind _ x k t)) (T.Rec s (Bind s' y _ u)) = T.Rec s $ Bind s' y k $ typeOnType t u
typeOnType _ t = t




-- collectAnn :: [K.Kind] -> E.Exp -> [K.Kind]
-- collectAnn xs (E.Abs _ _ (Bind _ x t e)) = collectAnn (collectAnnT xs t) e
-- collectAnn xs (E.App _ e1 e2) = collectAnn (collectAnn xs e1) e2
-- collectAnn xs (E.Pair _ e1 e2) = collectAnn (collectAnn xs e1) e2
-- collectAnn xs (E.BinLet _ _ _ e1 e2) = collectAnn (collectAnn xs e1) e2
-- collectAnn xs (E.Case _ e fm) = Map.foldl (\acc (_,e) -> collectAnn acc e) (collectAnn xs e) fm
-- collectAnn xs (E.TypeAbs _ b) = collectAnn (binder b : xs) (body b)
-- collectAnn xs (E.TypeApp _ e t) = collectAnnT (collectAnn xs e) t
-- collectAnn xs (E.Cond _ e1 e2 e3) = collectAnn (collectAnn (collectAnn xs e1) e2) e3
-- collectAnn xs (E.UnLet _ _ e1 e2) = collectAnn (collectAnn xs e1) e2
-- collectAnn xs _ = xs


-- collectAnnT :: [K.Kind] -> T.Type -> [K.Kind]
-- collectAnnT xs (T.Arrow _ _ t1 t2) = collectAnnT (collectAnnT xs t1) t2
-- collectAnnT xs (T.Labelled _ _ tm) = Map.foldl collectAnnT xs tm
-- collectAnnT xs (T.Semi _ t1 t2) = collectAnnT (collectAnnT xs t1) t2
-- collectAnnT xs (T.Message _ _ t) = collectAnnT xs t
-- collectAnnT xs (T.Forall _ b) = collectAnnT (binder b : xs) (body b)
-- collectAnnT xs (T.Rec _ b) = collectAnnT (binder b : xs) (body b)
-- collectAnnT xs (T.Dualof _ t) = collectAnnT xs t
-- collectAnnT xs _ = xs 














typeFromExp ::  VarEnv -> E.Exp -> T.Type
typeFromExp venv (E.Abs _ m (Bind s x t e)) = T.Arrow s m t (typeFromExp (Map.insert x t venv) e)
typeFromExp venv (E.TypeAbs _ (Bind s x k e)) =
--  trace ("\n>____> " ++ show x ++ "\t" ++ show k) $
  T.Forall s (Bind s x k (typeFromExp venv e))
typeFromExp venv v@(E.Var _ x) = case venv Map.!? x of
  Just t -> t -- trace ("\n>____> " ++ show x++ " : "++ show t)    t
  Nothing -> error $ "undefined " ++ show x 
typeFromExp venv (E.TypeApp s e t) =
  let (T.Forall s b) = typeFromExp venv e in subs t (var b) (body b)

typeFromExp varEnv (E.App s (E.Var _ x) e) | x == mkCollect s =
  evalState (let t = typeFromExp varEnv e in Extract.outChoiceMap e t >>= \tm -> return $
      T.Labelled s T.Variant (Map.map (T.Labelled s T.Record . Map.singleton (head mkTupleLabels p)) tm)) (initialState{varEnv})

typeFromExp varEnv (E.App s (E.App _ (E.Var _ x) (E.Var _ c)) e) | x == mkSelect s =
  evalState (let t = typeFromExp varEnv e in Extract.inChoiceMap e t >>= \m -> Extract.choiceBranch p m c t
            ) (initialState{varEnv})

typeFromExp varEnv (E.App p (E.App _ (E.Var _ x) e1) e2) | x == mkSend p = do
  evalState (let t = typeFromExp varEnv e2 in Extract.output e2 t >>= \(u1,u2) -> return u2) (initialState{varEnv})
                                                                 
typeFromExp varEnv (E.App s e1 e2) =  -- rType (typeFromExp venv e1)
  evalState (let t = typeFromExp varEnv e1 in Extract.function e1 t >>= \(_,_,u2) -> return u2) (initialState{varEnv})
--   let (T.Arrow _ _ _ t2)  = typeFromExp venv e1 in t2 

typeFromExp venv (E.Pair s e1 e2) =
  let t = typeFromExp venv e1
      u = typeFromExp venv e2
      (l0:l1:_) = mkTupleLabels in
  T.Labelled s T.Record (Map.insert (l0 s) t (Map.singleton (l1 s) u))

typeFromExp varEnv (E.BinLet s x y e1 e2) =
  evalState ( let t = typeFromExp varEnv e1 in Extract.pair e1 t >>= \(u1,u2) ->
                return $ typeFromExp (Map.insert x u1 (Map.insert y u2 varEnv)) e2
            ) (initialState{varEnv})
  
  -- let (T.Labelled _ T.Record m) = typeFromExp venv e1
  --     (l0:l1:_) = mkTupleLabels in
  --   typeFromExp (Map.insert x (m Map.! l0 s) (Map.insert y (m Map.! l1 s) venv)) e2
      
typeFromExp varEnv (E.Case    s e m) = -- TODO: Change toList
  -- fm'  <- buildMap p fm =<< Extract.datatypeMap e =<< synthetise kEnv e
  evalState (let t = typeFromExp varEnv e in (buildMap p m =<< Extract.datatypeMap e t)>>= \fm' ->
                return $ rType $ typeFromExp varEnv (snd $ snd $ head (Map.toList fm'))
            ) (initialState{varEnv})
--    typeFromExp venv e -- rType $ snd $ head (Map.toList m'')
typeFromExp venv (E.Cond _ _ e2 _) =  typeFromExp venv e2
typeFromExp venv (E.UnLet s x e1 e2) =
  let t = typeFromExp venv e1 in
    typeFromExp (Map.insert x t venv) e2
typeFromExp _ (E.Unit s) = T.unit s
typeFromExp _ (E.Int s _) = T.Int s
typeFromExp _ (E.Char s _) = T.Char s
typeFromExp _ (E.String s _) = T.String s
-- typeFromExp _
     
typeFromExp _ e = error $ ">>>> " ++ show e


rType (T.Arrow _ _ _ u) = rType u
rType u = u

 
-- individual tests

p = defaultSpan
varA = mkNewVar 199999 (mkVar p "a")
varB = mkNewVar 199998 (mkVar p "b")
varX = mkNewVar 199997 (mkVar p "x")
varF = mkNewVar 199996 (mkVar p "f")
varP = mkNewVar 199995 (mkVar p "p")
varWild = mkNewVar 199994 (mkVar p "_")

mkK i = K.KindVar defaultSpan $ mkVar defaultSpan $ "χ" ++ show i
mkM i = K.MultVar $ mkVar defaultSpan $ "φ" ++ show i

t1 = T.Forall p (Bind p varA (mkK 100) (T.Arrow p Un (T.Var p varA) (T.Var p varA)))

t2 = T.Forall p (Bind p varA (mkK 100) $
                  T.Forall p (Bind p varB (mkK 101) $
                                T.Arrow p Un alm (T.Var p varA)
                             )
                )
  where alm = T.tuple p [T.Var p varA, T.Var p varB]



idExp = E.TypeAbs p (Bind p varA (mkK 100) $
                     E.Abs p Un (Bind p varX (T.Var p varA) (E.Var p varX))
                    )


-- call with type f on venv
idExp2 = E.TypeAbs p (Bind p varA (mkK 100) $
                     E.Abs p Un (Bind p varX (T.Var p varA)
                                  (E.App p (E.App p 
                                    (E.TypeApp p (E.Var p varF) (T.Var p varA))
                                    (E.Var p varX)) (E.Var p varX))
                                )
                    )


fType = T.Forall p (Bind p varB (mkK 101)
                    (T.Arrow p Un (T.Var p varB)
                       (T.Arrow p Un (T.Var p varB) (T.Var p varB)))
                   )


--TODO: pair body

fstExp = E.TypeAbs p
           (Bind p varA (mkK 100) $
                E.TypeAbs p
                        (Bind p varB (mkK 101) $
                            E.Abs p Un (Bind p varP alm
                                            (E.BinLet p varX varWild (E.Var p varP) (E.Var p varX) )
                                       )
                        )
           )
  where alm = T.tuple p [T.Var p varA, T.Var p varB]


main = runTests--  >> putStrLn "\n------------------------------------------------------------\n" >>
  -- runProgs


-- runProgram :: FilePath -> IO ()
-- runProgram path = do
-- --  let path = "/Users/balmeida/workspaces/ContextFreeSession/FreeST/test/Programs/ValidTests/SessionTypes/sendTree/sendTree.fst"

--   preludeFp <- getDataFileName "Prelude.fst"
--   let s0 = initialState {runOpts=defaultOpts{runFilePath=preludeFp}}
--   when (hasErrors s0) (error $ show $ errors s0)
--   s1 <- parseProgram s0
--   let venv = Map.keysSet (noConstructors (typeEnv s1) (varEnv s1))
--   let penv = Map.keysSet (parseEnv s1)
--   let bs = Set.difference venv penv
--   s2 <- parseAndImport s1{builtins=bs, runOpts=(runOpts s1){runFilePath=path}}
--   when (hasErrors s2) (error $ show $ errors s2)
--   let kEnv = Map.map fst (typeEnv s2)
--   let f x = Map.notMember x (varEnv s1)

--   let s3 = execState ( fixConsTypes >> (checkNumArgs =<< getPEnvPat) >> (checkChanVar =<< getPEnvPat) >>
--                        (getPEnvPat >>= setPEnvPat . addMissingVars) >> ((matchFuns =<< getPEnvPat) >>= setPEnv) >>
--                        solveEquations >> ((resolve =<< getTEnv) >>= setTEnv) >> (elabVEnv =<< getVEnv) >>
--                        (elabPEnv =<< getPEnv) >> ((resolve =<< getVEnv) >>= setVEnv) >> ((resolve =<< getPEnv) >>= setPEnv)
--                      ) s2

--   -- this one is only to get the initial types & expressions 
--   let si =  emptyPEnv $ execState (buildProg >> renameState) s3
  
--   let ((vEnv, pEnv), state) = runState (do
--        m1 <- tMapWithKeyM (\k t -> if f k then cleanType (typeEnv s3) t else return t) (varEnv s3)
--        m2 <- tMapWithKeyM (\k (xs,e) -> if f k then (xs,) <$> cleanExp (typeEnv s3) e else return (xs,e)) (parseEnv s3)
--        return (m1, m2)) initial
        
--   let s4 = emptyPEnv $ execState (buildProg >> renameState) (s3{varEnv = vEnv, parseEnv = pEnv})

--   !s <- foldM (\acc (k, e) -> do
--           let !(sk,s) = runState (infGen kEnv e >>= \(_,u) -> traceM (show k ++ " "++ show u) >> infer k) (state{vEnv = varEnv s4})
          
--           let initialE = prog s4 Map.! k
--           let finalE = subsOnKE e (fst sk)
--           let initialT = varEnv s4 Map.! k
--           let finalT = subsOnKT (typeOnType (typeFromExp (varEnv s4) finalE) initialT) (fst sk)
--                 -- subsOnKT (typeFromExp (varEnv s4) finalE) (fst sk)
--           let iT = varEnv si Map.! k 
--           let iE = prog si Map.! k

--           -- let (test,res) = evalState (mapM (cg kEnv . snd) (typeEnv s4) >>= \tEnv ->
--           --                       infer k >>= \(sk,sm) ->                                   
--           --                       return (Map.mapWithKey (\x v ->
--           --                                 case v of
--           --                                   k@(K.Kind s (K.MultVar y) p) -> K.Kind s (Map.findWithDefault K.Lin y sm) p
--           --                                   k@(K.KindVar s v) -> Map.findWithDefault k v sk
--           --                                   k -> k
--           --                                 ) tEnv, sk)
--           --                      ) (state{vEnv = varEnv s4})
--           -- print  test
--           -- putStrLn $ "\n\n\n" ++ show res ++ "\n\n\n"
--           -- putStrLn $
--           --   "function " ++ show k ++  ":\n\t"
--           --        ++ "\nkConstraints:\n\t" ++ show (foldl cleanConstK [] (kConstraints s)) ++ "\n"
--           --        ++ "\nmConstraints:\n\t" ++ show (foldl cleanConstM [] (mConstraints s)) ++ "\n"
                 
--           --        -- ++ "\ntypeFromExp:\n\t" ++ show finalE ++ "\n\t"
--           --        --                         ++ show (typeFromExp (varEnv s4) finalE) ++ "\n"
                 
--           --        -- ++ "\ntype on type:\n\t" ++ show initialT ++ "\n\t"
--           --        --                          ++ show (typeFromExp (varEnv s4) finalE) ++ "\n\t"
--           --        --                          ++ show finalT ++ "\n"
                 
--           --        ++ "\nRESULT1:\n\t"       ++ show (fst sk) ++ "\n"
--           --        ++ "\nRESULT2:\n\t"       ++ show (snd sk) ++ "\n"

--           --        ++ "\nOriginal:\n\t"  ++ show k ++ " : " ++ show iT ++ "\n\t"
--           --                          ++ show k ++ " = " ++ show iE ++ "\n"
                                   
--           --        ++ "\nCleaned:\n\t" ++ show k ++ " : " ++ show (vEnv Map.! k) ++ "\n\t"
--           --                          ++ show k ++ " = " ++ show (snd (pEnv Map.! k)) ++ "\n"

--           --        -- ++ "\nRenamed:\n\t" ++ show k ++ " : " ++ show (varEnv s4 Map.! k) ++ "\n\t"
--           --        --                   ++ show k ++ " = " ++ show (prog s4 Map.! k) ++ "\n"
                                   
--           --        ++ "\nAltered:\n\t" ++ show k ++ " : " ++ show finalT ++ "\n\t"
--           --                          ++ show k ++ " = " ++ show finalE ++ "\n\t"                                  
--           --        ++ "\n---------------------\n"

          
          
--           if show iT /= show finalT || show iE /= show finalE
--             then return $ (k,iT,iE, finalT, finalE) : acc
--             else return acc
          
--           ) [] (Map.toList $ Map.filterWithKey (\k _ -> f k) (prog s4))

--   putStrLn path
--   !s <- mapM_ (\(k,it,ie,ft,fe) -> -- putStrLn (show k ++ "\n") >>
--                 putStrLn (show k ++ " : " ++ show it ++ "\n" ++
--                           show k ++ " = " ++ show ie ++ "\n\n" ++
--                           show k ++ " : " ++ show ft ++ "\n" ++
--                           show k ++ " = " ++ show fe ++ "\n\n"                          
--                          ) >> putStrLn "--------------------\n"
--               ) s
--   --  let final = subsOnKE e' (fst r)
--   return ()

