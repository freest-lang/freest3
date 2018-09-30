module CodeGen.CodeGen
 (
   genProgram
 , HaskellCode(..)
 ) where 

import           CodeGen.DatatypeGen
import           CodeGen.ExpressionGen
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Directory
import           Syntax.Terms
import           Validation.Kinding
import           Validation.TypingState(KindEnv)
import           Syntax.Types


-- TODO : PARAM BANG
genProgram :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO ()
genProgram venv eenv cenv kenv path = do
  genUtils path
  let startType  = last $ toList $ venv Map.! "start"
      dataTypes  = genDataTypes cenv
      file       = genFile eenv
      mainFun    = genMain eenv (eenv Map.! "start") startType in
      writeFile (path ++ "cfst.hs") (genPragmas ++ genImports ++ dataTypes ++ file ++ mainFun)



genFile :: ExpEnv -> HaskellCode
genFile eenv =
  Map.foldrWithKey (\f (p, e) acc ->
                      acc ++ f ++ " " ++ showBangParams p ++ " = " ++
                      code f e p ++ "\n\n") "" eenv
  where 
    code f e p =
      let m = monadicFuns eenv
          m1 = foldr (\x acc -> Map.insert x False acc) m p
          m2 = fst $ isMonadic m1 (m1 Map.! f) Map.empty e in
      fst $ evalState (translate m1 m2 e) 0 

showBangParams :: Params -> String
showBangParams [] = ""
showBangParams args = "!" ++ intercalate " !" args

genImports :: String
genImports = "import CFSTUtils\n\n"

genPragmas :: String
genPragmas = "{-# LANGUAGE BangPatterns #-}\n\n"

genMain :: ExpEnv  -> (Params, Expression) -> TypeScheme -> HaskellCode
genMain eenv (params, startExp) t =  
  let m = monadicFuns eenv
-- Main doesn't have parens
--      m1 = foldr (\x acc -> Map.insert x False acc) m params
--      (m2, b1) = isMonadic m1 (m1 Map.! "start") Map.empty startExp
      (m2, b1) = isMonadic m (m Map.! "start") Map.empty startExp
      (h,b) = evalState (translate m m2 startExp) 0 in
--      (h,b) = evalState (translate m1 m2 startExp) 0 in
  if b || b1 then
    "main = start >>= \\res -> putStrLn (show (res :: " ++ show t ++ "))\n\n"
  else
    "main = putStrLn (show start)\n\n"

-- GENERATES THE COMMUNICATION AND THREAD CREATION MODULE

-- genUtils :: FilePath -> IO ()
-- genUtils path = do
--   b <- doesFileExist (path ++ "CFSTUtils.hs")
--   if b then return () else genUtilsFile (path ++ "CFSTUtils.hs")

-- genUtilsFile :: FilePath -> IO ()
genUtils :: FilePath -> IO ()
genUtils path =
  writeFile (path ++ "CFSTUtils.hs")
    ("module CFSTUtils (_fork, _new, _send, _receive) where\n\n" ++
     genUtilsImports ++ "\n\n" ++
     genFork ++ "\n\n" ++ genNew ++ "\n\n" ++
     genSend ++ "\n\n" ++ genReceive)

genUtilsImports :: String
genUtilsImports =
  "import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)\nimport Unsafe.Coerce\n\n"


genFork :: String
genFork = "_fork e = do\n  forkIO e\n  return ()"

genNew :: String
genNew = "_new = do\n  m1 <- newEmptyMVar\n  m2 <- newEmptyMVar\n  return ((m1, m2), (m2, m1))"

genSend :: String
genSend = "_send x (m1, m2) = do\n  putMVar m2 (unsafeCoerce x)\n  return (m1, m2)"

genReceive :: String
genReceive = "_receive (m1, m2) = do\n  a <- takeMVar m1\n  return ((unsafeCoerce a), (m1, m2))"

{- With channels

genUtilsImports :: String
genUtilsImports =
  "import Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan.Synchronous\nimport Unsafe.Coerce"

genNew :: String
genNew = "new = do\n  ch <- newChan\n  return (ch, ch)"

genSend :: String
genSend = "send x ch  = do\n  writeChan ch (unsafeCoerce x)\n  return ch"

genReceive :: String
genReceive = "receive ch = do\n  a <- readChan ch\n  return ((unsafeCoerce a), ch)"
-}
  
-- -- TESTING
-- -- Must to import Kinds

-- t str =
--   let m0 = monadicFuns t13
--     --  m1 = foldr (\x acc -> Map.insert x False acc) m0 (fst (t11 Map.! "start"))
--       m2 = fst $ isMonadic m0 False Map.empty (snd (t13 Map.! str)) in
--       fst $ evalState (translate m0 m2 (snd (t13 Map.! str))) 0

-- t' srt= let m = monadicFuns t11 in fst $ isMonadic m False Map.empty (snd (t13 Map.! srt))

-- tester :: ExpEnv -> IO ()
-- tester eenv = putStrLn $ tester' eenv

-- tester' :: ExpEnv -> String
-- tester' eenv =
--   Map.foldrWithKey (\f (p, e) acc ->
--                       acc ++ f ++ " " ++ showBangParams p ++ " = " ++ code f e p ++ "\n\n") "" eenv
--   where 
--     code f e p =
--       let m0 = monadicFuns eenv
--           m1 = foldr (\x acc -> Map.insert x False acc) m0 p
--           m2 = fst $ isMonadic m1 (m1 Map.! f) Map.empty e in
--       fst $ evalState (translate m1 m2 e) 0 




-- -- OK
-- -- let x = 2 in x
-- t0 = Map.fromList [("f1", ([], (UnLet (12,11) "val" (Integer (1,2) 2) (Variable (0,0) "val"))))]

   

-- -- OK
-- -- let x = send (2+2) c in x
-- t2 = Map.fromList [("f1", ([], (UnLet (12,11) "val1" (Send (12,20) (App (0,0) (App (0,0) (Variable (0,0) "(+)") (Integer (0,0) 2)) (Integer (0,0) 2)) (Variable (0,0) "c")) (Variable (0,0) "val1"))))]

-- -- OK
-- -- let x = send 5 c in x
-- t3 = Map.fromList [("f1", ([], (UnLet (12,11) "zzz" (Send (12,20) (Integer (1,2) 5) (Variable (0,0) "c"))) (Variable (0,0) "zzz")))]


-- -- Simple fork
-- -- let x = fork (client w) in x
-- t4 = Map.fromList [("client",(["x"],Variable (10,12) "x")),("f1",(["w"],UnLet (3,7) "x" (Fork (3,16) (App (3,17) (Variable (3,17) "client") (Variable (3,24) "w"))) (Variable (4,3) "x")))]

-- -- start = let w,r = (3,True) in r
-- t5 = Map.fromList [("start",([],BinLet (12,7) "w" "r" (Pair (12,14) (Integer (12,14) 3) (Boolean (12,16) True)) (Variable (13,3) "r")))]

-- -- OUT: start = new >>= \(w,r) -> fork ((boolServer r)) >>= \x -> return (client1 w)
-- t6 = Map.fromList [("start",([],BinLet (15,7) "w" "r" (New (15,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (16,7) "x" (Fork (16,16) (App (16,17) (Variable (16,17) "boolServer") (Variable (16,28) "r"))) (App (17,3) (Variable (17,3) "client") (Variable (17,11) "w")))))]

-- -- with client1
-- -- OUT: ... client1 w ....
-- t7 = Map.fromList [("client1",(["w"],UnLet (21,7) "w1" (Select (21,19) "And" (Variable (21,23) "w")) (UnLet (22,7) "w2" (Send (22,17) (Boolean (22,17) True) (Variable (22,22) "w1")) (UnLet (23,7) "r1" (Send (23,17) (Boolean (23,17) False) (Variable (23,23) "w2")) (BinLet (24,7) "x" "r2" (Receive (24,23) (Variable (24,23) "r1")) (Variable (25,3) "x")))))),("start",([],BinLet (15,7) "w" "r" (New (15,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (16,7) "x" (Fork (16,16) (App (16,17) (Variable (16,17) "boolServer") (Variable (16,28) "r"))) (App (17,3) (Variable (17,3) "client1") (Variable (17,11) "w")))))]

-- -- Boolserver original
-- t8 = Map.fromList [("boolServer",(["c"],Match (31,9) (Variable (31,9) "c") (Map.fromList [("And",("c1",BinLet (33,11) "n1" "c2" (Receive (33,28) (Variable (33,28) "c1")) (BinLet (34,11) "n2" "c3" (Receive (34,28) (Variable (34,28) "c2")) (UnLet (35,11) "x" (Send (35,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (35,21) "n1")) (Variable (35,27) "n2")) (Variable (35,31) "c3")) (Unit (36,7)))))),("Not",("c1",BinLet (45,11) "n1" "c2" (Receive (45,28) (Variable (45,28) "c1")) (UnLet (46,11) "x" (Send (46,20) (App (0,0) (Variable (0,0) "not") (Variable (46,25) "n1")) (Variable (46,29) "c2")) (Unit (47,7))))),("Or",("c1",BinLet (39,11) "n1" "c2" (Receive (39,28) (Variable (39,28) "c1")) (BinLet (40,11) "n2" "c3" (Receive (40,28) (Variable (40,28) "c2")) (UnLet (41,11) "x" (Send (41,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (41,21) "n1")) (Variable (41,27) "n2")) (Variable (41,31) "c3")) (Unit (42,7))))))]))),("client1",(["w"],UnLet (21,7) "w1" (Select (21,19) "And" (Variable (21,23) "w")) (UnLet (22,7) "w2" (Send (22,17) (Boolean (22,17) True) (Variable (22,22) "w1")) (UnLet (23,7) "r1" (Send (23,17) (Boolean (23,17) False) (Variable (23,23) "w2")) (BinLet (24,7) "x" "r2" (Receive (24,23) (Variable (24,23) "r1")) (Variable (25,3) "x")))))),("start",([],BinLet (15,7) "w" "r" (New (15,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (16,7) "x" (Fork (16,16) (App (16,17) (Variable (16,17) "boolServer") (Variable (16,28) "r"))) (App (17,3) (Variable (17,3) "client1") (Variable (17,11) "w")))))]

-- -- f1 c = send 5 c >>= \zzz -> return ()
-- t9 = Map.fromList [("f1", (["c"], (UnLet (12,11) "zzz" (Send (12,20) (Integer (1,2) 5) (Variable (0,0) "c"))) (Unit (21,0))))]

-- -- (Match (31,9) (Variable (31,9) "c") (fromList [("And",("c1",BinLet (33,11) "n1" "c2" (Receive (33,28) (Variable (33,28) "c1")) (BinLet (34,11) "n2" "c3" (Receive (34,28) (Variable (34,28) "c2")) (UnLet (35,11) "x" (Send (35,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (35,21) "n1")) (Variable (35,27) "n2")) (Variable (35,31) "c3")) (Unit (36,7)))))),("Not",("c1",BinLet (45,11) "n1" "c2" (Receive (45,28) (Variable (45,28) "c1")) (UnLet (46,11) "x" (Send (46,20) (App (0,0) (Variable (0,0) "not") (Variable (46,25) "n1")) (Variable (46,29) "c2")) (Unit (47,7))))),("Or",("c1",BinLet (39,11) "n1" "c2" (Receive (39,28) (Variable (39,28) "c1")) (BinLet (40,11) "n2" "c3" (Receive (40,28) (Variable (40,28) "c2")) (UnLet (41,11) "x" (Send (41,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (41,21) "n1")) (Variable (41,27) "n2")) (Variable (41,31) "c3")) (Unit (42,7))))))]),True)

-- t10 = Map.fromList [("boolServer",(["c"],Match (5,9) (Variable (5,9) "c") (Map.fromList [("And",("c1",BinLet (7,11) "n1" "c2" (Receive (7,28) (Variable (7,28) "c1")) (BinLet (8,11) "n2" "c3" (Receive (8,28) (Variable (8,28) "c2")) (UnLet (9,11) "x" (Send (9,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (9,21) "n1")) (Variable (9,27) "n2")) (Variable (9,31) "c3")) (Unit (10,7)))))),("Not",("c1",BinLet (19,11) "n1" "c2" (Receive (19,28) (Variable (19,28) "c1")) (UnLet (20,11) "x" (Send (20,20) (App (0,0) (Variable (0,0) "not") (Variable (20,25) "n1")) (Variable (20,29) "c2")) (Unit (21,7))))),("Or",("c1",BinLet (13,11) "n1" "c2" (Receive (13,28) (Variable (13,28) "c1")) (BinLet (14,11) "n2" "c3" (Receive (14,28) (Variable (14,28) "c2")) (UnLet (15,11) "x" (Send (15,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (15,21) "n1")) (Variable (15,27) "n2")) (Variable (15,31) "c3")) (Unit (16,7))))))]))),("client1",(["w"],UnLet (29,7) "w1" (Select (29,19) "And" (Variable (29,23) "w")) (UnLet (30,7) "w2" (Send (30,17) (Boolean (30,17) True) (Variable (30,22) "w1")) (UnLet (31,7) "r1" (Send (31,17) (Boolean (31,17) False) (Variable (31,23) "w2")) (BinLet (32,7) "x" "r2" (Receive (32,23) (Variable (32,23) "r1")) (Variable (33,3) "x")))))),("start",([],App (25,9) (Variable (25,9) "startClient") (Variable (25,21) "client1"))),("startClient",(["client"],BinLet (38,7) "w" "r" (New (38,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (39,7) "x" (Fork (39,16) (App (39,17) (Variable (39,17) "boolServer") (Variable (39,28) "r"))) (App (40,3) (Variable (40,3) "client") (Variable (40,10) "w")))))]


-- t11 = Map.fromList [("boolServer",(["c"],Match (5,9) (Variable (5,9) "c") (Map.fromList [("And",("c1",BinLet (7,11) "n1" "c2" (Receive (7,28) (Variable (7,28) "c1")) (BinLet (8,11) "n2" "c3" (Receive (8,28) (Variable (8,28) "c2")) (UnLet (9,11) "x" (Send (9,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (9,21) "n1")) (Variable (9,27) "n2")) (Variable (9,31) "c3")) (Unit (10,7)))))),("Not",("c1",BinLet (19,11) "n1" "c2" (Receive (19,28) (Variable (19,28) "c1")) (UnLet (20,11) "x" (Send (20,20) (App (0,0) (Variable (0,0) "not") (Variable (20,25) "n1")) (Variable (20,29) "c2")) (Unit (21,7))))),("Or",("c1",BinLet (13,11) "n1" "c2" (Receive (13,28) (Variable (13,28) "c1")) (BinLet (14,11) "n2" "c3" (Receive (14,28) (Variable (14,28) "c2")) (UnLet (15,11) "x" (Send (15,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (15,21) "n1")) (Variable (15,27) "n2")) (Variable (15,31) "c3")) (Unit (16,7))))))]))),("client1",(["w"],UnLet (32,7) "w1" (Select (32,19) "And" (Variable (32,23) "w")) (UnLet (33,7) "w2" (Send (33,17) (Boolean (33,17) True) (Variable (33,22) "w1")) (UnLet (34,7) "r1" (Send (34,17) (Boolean (34,17) False) (Variable (34,23) "w2")) (BinLet (35,7) "x" "r2" (Receive (35,23) (Variable (35,23) "r1")) (Variable (36,3) "x")))))),("client2",(["w"],UnLet (40,7) "w1" (Select (40,19) "Not" (Variable (40,23) "w")) (UnLet (41,7) "r1" (Send (41,17) (Boolean (41,17) True) (Variable (41,22) "w1")) (BinLet (42,7) "x" "r2" (Receive (42,23) (Variable (42,23) "r1")) (Variable (43,3) "x"))))),("start",([],UnLet (25,7) "c1" (App (25,12) (Variable (25,12) "startClient") (Variable (25,24) "client1")) (UnLet (26,7) "c2" (App (26,12) (Variable (26,12) "startClient") (Variable (26,24) "client2")) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (27,3) "c1")) (Variable (27,9) "c2"))))),("startClient",(["client"],BinLet (48,7) "w" "r" (New (48,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (49,7) "x" (Fork (49,16) (App (49,17) (Variable (49,17) "boolServer") (Variable (49,28) "r"))) (App (50,3) (Variable (50,3) "client") (Variable (50,10) "w")))))]

-- t12 = Map.fromList [("sendTree",(["t","c"],Case (13,8) (Variable (13,8) "t") (Map.fromList [("Leaf",([],UnLet (15,11) "x" (Select (15,22) "LeafC" (Variable (15,28) "c")) (Unit (16,7)))),("Node",(["x","l","r"],UnLet (18,12) "x" (Select (18,23) "LeafC" (Variable (18,29) "c")) (Unit (19,7))))]))),("start",([],Integer (70,9) 10))]

-- t13 = Map.fromList [("receiveOne",(["c"],Match (40,9) (Variable (40,9) "c") (Map.fromList [("LeafC",("c1",Pair (42,8) (Constructor (42,13) "LeafA") (Constructor (42,20) "LeafA"))),("NodeC",("c1",BinLet (44,11) "x" "c2" (Receive (44,27) (Variable (44,27) "c1")) (BinLet (45,11) "left" "c3" (App (45,22) (Variable (45,22) "receiveOne") (Variable (45,33) "c2")) (Pair (47,8) (App (47,8) (App (47,8) (Constructor (47,8) "NodeA") (Variable (47,14) "x")) (Variable (47,16) "left")) (Variable (47,22) "left")))))]))),("sendOne",(["t","c"],Case (27,8) (Variable (27,8) "t") (Map.fromList [("LeafA",([],UnLet (29,11) "x" (Select (29,22) "LeafC" (Variable (29,28) "c")) (Unit (30,7)))),("NodeA",(["x","l"],UnLet (32,11) "w1" (Select (32,23) "NodeC" (Variable (32,29) "c")) (UnLet (33,11) "w2" (Send (33,21) (Variable (33,21) "x") (Variable (33,23) "w1")) (UnLet (34,11) "w3" (App (34,16) (App (34,16) (Variable (34,16) "sendOne") (Variable (34,24) "l")) (Variable (34,26) "w2")) (Unit (36,7))))))]))),("start",([],UnLet (113,6) "inTree" (App (113,15) (App (113,15) (Constructor (113,15) "NodeA") (Integer (113,21) 7)) (Constructor (113,29) "LeafA")) (BinLet (116,6) "writer" "reader" (New (116,26) (Rec (Bind {var = "x", kind = Kind {prekind = Session, multiplicity = Un}}) (Choice Internal (Map.fromList [("LeafC",Skip),("NodeC",Semi (Message Out IntType) (Var "x"))])))) (UnLet (117,6) "w" (Fork (117,15) (App (117,16) (App (117,16) (Variable (117,16) "sendOne") (Variable (117,24) "inTree")) (Variable (117,31) "writer"))) (BinLet (118,6) "outTree" "r" (App (118,19) (Variable (118,19) "receiveOne") (Variable (118,30) "reader")) (Variable (119,2) "outTree"))))))]

-- -- BoolServer 2
-- t14 = Map.fromList [("boolServer",(["c"],Match (5,9) (Variable (5,9) "c") (Map.fromList [("And",("c1",BinLet (7,11) "n1" "c2" (Receive (7,28) (Variable (7,28) "c1")) (BinLet (8,11) "n2" "c3" (Receive (8,28) (Variable (8,28) "c2")) (UnLet (9,11) "x" (Send (9,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (9,21) "n1")) (Variable (9,27) "n2")) (Variable (9,31) "c3")) (Unit (10,7)))))),("Not",("c1",BinLet (19,11) "n1" "c2" (Receive (19,28) (Variable (19,28) "c1")) (UnLet (20,11) "x" (Send (20,20) (App (0,0) (Variable (0,0) "not") (Variable (20,25) "n1")) (Variable (20,29) "c2")) (Unit (21,7))))),("Or",("c1",BinLet (13,11) "n1" "c2" (Receive (13,28) (Variable (13,28) "c1")) (BinLet (14,11) "n2" "c3" (Receive (14,28) (Variable (14,28) "c2")) (UnLet (15,11) "x" (Send (15,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (15,21) "n1")) (Variable (15,27) "n2")) (Variable (15,31) "c3")) (Unit (16,7))))))]))),("client1",(["w"],UnLet (32,7) "w1" (Select (32,19) "And" (Variable (32,23) "w")) (UnLet (33,7) "w2" (Send (33,17) (Boolean (33,17) True) (Variable (33,22) "w1")) (UnLet (34,7) "r1" (Send (34,17) (Boolean (34,17) False) (Variable (34,23) "w2")) (BinLet (35,7) "x" "r2" (Receive (35,23) (Variable (35,23) "r1")) (Variable (36,3) "x")))))),("client2",(["w"],UnLet (40,7) "w1" (Select (40,19) "Not" (Variable (40,23) "w")) (UnLet (41,7) "r1" (Send (41,17) (Boolean (41,17) True) (Variable (41,22) "w1")) (BinLet (42,7) "x" "r2" (Receive (42,23) (Variable (42,23) "r1")) (Variable (43,3) "x"))))),("start",([],UnLet (25,7) "c1" (App (25,12) (Variable (25,12) "startClient") (Variable (25,24) "client1")) (UnLet (26,7) "c2" (App (26,12) (Variable (26,12) "startClient") (Variable (26,24) "client2")) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (27,3) "c1")) (Variable (27,9) "c2"))))),("startClient",(["client"],BinLet (48,7) "w" "r" (New (48,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (49,7) "x" (Fork (49,16) (App (49,17) (Variable (49,17) "boolServer") (Variable (49,28) "r"))) (App (50,3) (Variable (50,3) "client") (Variable (50,10) "w")))))]

-- -- intList
-- intList = Map.fromList [("length'",(["l"],Case (5,8) (Variable (5,8) "l") (Map.fromList [("Cons",(["x","y"],App (0,0) (App (0,0) (Variable (0,0) "(+)") (Integer (7,17) 1)) (App (7,21) (Variable (7,21) "length'") (Variable (7,29) "y")))),("Nil",([],Integer (6,12) 0))]))),("start",([],App (11,9) (Variable (11,9) "length'") (App (11,18) (App (11,18) (Constructor (11,18) "Cons") (Integer (11,23) 5)) (App (11,26) (App (11,26) (Constructor (11,26) "Cons") (Integer (11,31) 7)) (App (11,34) (App (11,34) (Constructor (11,34) "Cons") (Integer (11,39) 23)) (App (11,43) (App (11,43) (Constructor (11,43) "Cons") (Integer (11,48) 4)) (Constructor (11,53) "Nil")))))))]

