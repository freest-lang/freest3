module CodeGen.CodeGen1
 (
   HaskellCode(..)
 , MonadicMap
 , genProgram
 ) where 

import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           Terms.Terms
import           Types.Kinding
import           Types.Kinds
import           System.Directory
import  Types.Types -- TODO: Delete
  
-- 1ST PASSAGE

-- After this passage each node of the AST is annotated
-- with a Boolean value that gives the information of the
-- monadic form of the node

type MonadicMap = Map.Map Expression Bool

-- Calls the isMonadic function for each expression in the environment

isMonadicEnv :: ExpEnv -> MonadicMap
isMonadicEnv = Map.foldl (\acc x -> menv acc x) Map.empty
   where
     menv :: MonadicMap -> (Params, Expression) -> MonadicMap
     menv acc (p,x) = Map.union (fst (isMonadic False acc x)) (monadicParams p)
     
     monadicParams :: Params -> MonadicMap
     monadicParams = foldr (\x acc -> fst $ (isMonadic False acc (Variable (3,24) x))) Map.empty

-- This function checks if an expression is in the monadic form
-- and then updates the monadic map with that information.

isMonadic :: Bool -> MonadicMap -> Expression -> (MonadicMap, Bool)
isMonadic b m (Unit p) = (Map.insert (Unit p) b m, b)
isMonadic b m (Integer p i) = (Map.insert (Integer p i) b m, b)
isMonadic b m (Character p c) = (Map.insert (Character p c) b m, b)
isMonadic b' m (Boolean p b) = (Map.insert (Boolean p b) b' m, b')
isMonadic b m (Variable p x) = (Map.insert (Variable p x) b m, b)
isMonadic b m (UnLet p x e1 e2) =
  let (m1, b1) = isMonadic b m e1
      (m2, b2) = isMonadic b1 m1 e2 in
--      (Map.insert (UnLet p x e1 e2) b2 m2, b2)
      (Map.insert (UnLet p x e1 e2) (b1 || b2) m2, b2)
     
isMonadic b m (App p e1 e2) = 
  let (m1, b1) = isMonadic False m e1
      (m2, b2) = isMonadic False m1 e2 in
--      (Map.insert (App p e1 e2) False m2, False)
      (Map.insert (App p e1 e2) (b1 || b2) m2, b1 || b2)

isMonadic b m (TypeApp p e ts) = 
  let (m1, b1) = isMonadic b m e in
      (Map.insert (TypeApp p e ts) b1 m1, b1)

isMonadic b m (Conditional p e1 e2 e3) =
  let (m1, _) = isMonadic False m e1
      (m2, _) = isMonadic b m1 e2
      (m3, b1) = isMonadic b m2 e3 in
    (Map.insert (Conditional p e1 e2 e3) b1 m3, b1)
      
isMonadic b m (Pair p e1 e2)  = 
  let (m1, _) = isMonadic False m e1
      (m2, _) = isMonadic False m1 e2 in
      (Map.insert (Pair p e1 e2) False m2, False)

isMonadic b m (BinLet p x y e1 e2)  = 
  let (m1, b1) = isMonadic b m e1
      (m2, b2) = isMonadic b1 m1 e2 in
      (Map.insert (BinLet p x y e1 e2) (b1 || b2) m2, b1 || b2)
      
isMonadic b m (New p t) = (Map.insert (New p t) True m, True)

isMonadic b m (Send p e1 e2) =
  let (m1, _) = isMonadic False m e1
      (m2, _) = isMonadic False m1 e2 in
      (Map.insert (Send p e1 e2) True m2, True)
      
isMonadic b m (Receive p e) =  
  let (m1,_) = isMonadic False m e in
      (Map.insert (Receive p e) True m1, True)
      
isMonadic b m (Select p x e) =  
  let (m1,_) = isMonadic False m e in
      (Map.insert (Select p x e) True m1, True)
      
isMonadic b m (Match p e mmap) =
  let m1 = isMapMonadic m mmap in
      (Map.insert (Match p e mmap) True m1, True)

isMonadic b m (Fork p e) = (Map.insert (Fork p e) True m, True)

isMonadic b m (Constructor p x) = (Map.insert (Constructor p x) False m, False)

isMonadic b m (Case p e cm) = 
  let m1 = isMapMonadic m cm in
      (Map.insert (Case p e cm) False m, False) 

isMapMonadic :: MonadicMap -> Map.Map a (b, Expression) -> MonadicMap
isMapMonadic m mmap = Map.foldr (\x acc -> fst $ isMonadic True acc (snd x)) m mmap

{- Was
isMapMonadic :: MonadicMap -> Map.Map a (b, Expression) -> (MonadicMap, Bool)
isMapMonadic m mmap = head $
  Map.foldl (\acc (_, e) -> acc ++ [isMonadic m e]) [] mmap
-}


-- 2ND PASSAGE

type HaskellCode = String
type TranslateMonad = State Int

-- Gets the next fresh var based on the state
nextFresh :: TranslateMonad String
nextFresh = do
  fresh <- get
  modify (+1)
  return $ "_x" ++ show fresh

-- EXPRESSIONS TRANSLATION

translateExpr :: HaskellCode -> Bool -> Bool -> TranslateMonad HaskellCode
translateExpr c expected found
  | found == expected = return c
  | expected          = return $ "return " ++ c
  | otherwise         = return $ c ++ " >>= \\_x -> _x" -- FRESH HERE

expected :: MonadicMap -> Expression -> Bool
expected m e
  | Map.member e m = m Map.! e
  | otherwise      = False

{- Was:
translateExpr :: HaskellCode -> MonadicMap -> Expression -> Bool -> TranslateMonad HaskellCode
translateExpr c m e found
  | found == (expected m e) = return c
  | expected m e            = return $ "return " ++ c
  | otherwise               = return $ c ++ " >>= \\_x -> _x" -- FRESH HERE
-}

--

translate :: MonadicMap -> Expression -> TranslateMonad (HaskellCode, Bool)
translate m (Unit p) = do
  h <- translateExpr "()" (expected m (Unit p)) False
  return (h, False)
  
translate m (Integer p i) = do
  h <- translateExpr (show i) (expected m (Integer p i)) False
  return (h, False)
  
translate m (Character p c) = do
  h <- translateExpr (show c) (expected m (Character p c)) False
  return (h, False)
  
translate m (Boolean p b) = do
  h <- translateExpr (show b) (expected m (Boolean p b)) False
  return (h, False)

translate m (Variable p x) = do
  h <- translateExpr x (expected m (Variable p x)) False
  return (h, False)
  
translate m (UnLet p x e1 e2) = do 
  (h1, b1) <- translate m e1
  (h2, b2) <- translate m e2

--  c1 <- translateExpr h1 (expected m e1) b1
--  c2 <- translateExpr h2 (expected m (UnLet p x e1 e2)) b2

-- --  c2 <- translateExpr h2 m e2 b2
-- --  let c2 = translateExpr h2 (expected m e2) b2

  if b1 || b2 then
    return (h1 ++ " >>= \\" ++ x ++ " -> " ++ h2, True) 
  else
    return ("let " ++ x ++ " = " ++ h1 ++ " in " ++ h2, b2)
--  return (translateExpr (c1 ++ " " ++ c2) (expected m (UnLet p x e1 e2)) b2, b2)

translate m (App p e1 e2) = do
  (h1, b1) <- translate m e1
  (h2, b2) <- translate m e2

  --checkFun m (h1,b1) (h2,b2) e1 e2

  c <- translateExpr ("(" ++ h1 ++ " " ++ h2 ++ ")") (expected m e1) (b1||b2)
  return (c, b1 || b2)

-- TODO:
translate m (TypeApp p e ts) = translate m e

translate m (Conditional _ c e1 e2) = do
  (b1, _) <- translate m c
  (h1, b2) <- translate m e1
  (h2, b3) <- translate m e2

--  c1 <- translateExpr h1 (expected m e1) b2
--  c2 <- translateExpr h2 (expected m e2) b3
  
  return ("if " ++ b1 ++ " then " ++ h1 ++ " else " ++ h2, b2 || b3)

translate m (Pair _ e1 e2) = do
  (h1, b1) <- translate m e1
  (h2, b2) <- translate m e2
  return ("(" ++ h1 ++ ", " ++ h2 ++ ")", False)

translate m (BinLet p x y e1 e2) = do
  (h1, b1) <- translate m e1
  (h2, b2) <- translate m e2

 -- c1 <- translateExpr h1 (expected m e1) b1
 -- c2 <- translateExpr h2 (expected m (BinLet p x y e1 e2)) b2
--  c2 <- translateExpr h2 m e2 b2
  
  if b1 then
    return (h1  ++ " >>= \\(" ++ x ++ ", " ++ y ++ ")" ++ " -> " ++ h2, True) 
  else
    return ("let (" ++ x ++ ", " ++ y ++ ")" ++ " = " ++ h1 ++ " in " ++ h2, b2)
  
translate m (New _ _) = return ("new", True)

translate m (Send p e1 e2) = do
  (h1, _) <- translate m e1
  (h2, _) <- translate m e2
  c <- translateExpr ("send " ++ h1 ++ " " ++ h2) (expected m (Send p e1 e2)) True
  return (c, True)
--  return ("send " ++ h1 ++ " " ++ h2, True)

translate m (Receive p e) = do
  (h, b) <- translate m e
  c <- translateExpr h (expected m e) b -- TRUE?

  if b then
    do
      v <- nextFresh
      return ("\\" ++ v ++ " -> " ++ "receive " ++ c, True)
  else
    return ("receive " ++ c, True)

translate m (Select p x e) = do
  (h, _) <- translate m e
  c <- translateExpr ("send \"" ++ x ++ "\" " ++ h) (expected m (Select p x e)) True
  return (c, True)  

translate m (Match p e mm) = do
  (h1, b1) <- translate m e
  (h2, params) <- translateMatchMap m mm
  v <- nextFresh
  return ("receive " ++ h1 ++ " >>= \\(" ++ v ++  ", " ++ (head params) ++
          ") -> case " ++ v ++ " of " ++ h2, False)

translate m (Fork p e) = do
  (h1, b1) <- translate m e
  c1 <- translateExpr ("fork (" ++ h1 ++ ")") (expected m (Fork p e)) True
  return (c1, True)
  
translate m (Constructor p x) = do
  h <- translateExpr x (expected m (Constructor p x)) False
  return (h, False)

translate m (Case p e cm) = do
  (h1,_) <- translate m e 
  hcase <- translateCaseMap m cm
  return ("case " ++ h1 ++ " of " ++ hcase, False)
  
-- TODO: Join with case
translateMatchMap :: MonadicMap -> MatchMap -> TranslateMonad (String, [String])
translateMatchMap m = Map.foldlWithKey (translateMatchMap' m) (return ("", []))
  where
    translateMatchMap' m acc v (param, e) = do
      (h, b) <- translate m e
      acc' <- acc
      return (fst acc' ++ "\n    \"" ++ v ++ "\" " ++ " -> " ++ h
             , snd acc' ++ [param])

translateCaseMap :: MonadicMap -> CaseMap -> TranslateMonad String
translateCaseMap m = Map.foldlWithKey (translateCaseMap' m) (return "")
  where
    translateCaseMap' m acc v (params, e) = do
      (h1, _) <- translate m e
      acc' <- acc
      return (acc' ++ "\n    " ++ v ++ showParams params ++ " -> " ++ h1 ++ " ")

-- TODO : PARAM BANG
genProgram :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO ()
genProgram venv eenv cenv kenv path = do
  genCFSTComm path
  let start = eenv Map.! "start"
  let startType = last $ toList $ venv Map.! "start"
  let monadicMap = isMonadicEnv eenv
  let dataTypes = showDT (genDataType cenv kenv)
  let file = genFile monadicMap eenv
  let mainFun = genMain eenv monadicMap start startType
  writeFile (path ++ "cfst.hs") (genPragmas ++ genImports ++ dataTypes ++ file ++ mainFun)
    

genFile :: MonadicMap -> ExpEnv -> HaskellCode
genFile m eenv =
  Map.foldrWithKey (\f (p, e) acc ->
                      acc ++ f ++ showParams p ++ " = " ++
                      code e ++ "\n\n") "" eenv
  where 
    code e = fst $ evalState (translate m e) 0 



genImports :: String
genImports = "import CFSTCommunication\n\n"

genPragmas :: String
genPragmas = "{-# LANGUAGE BangPatterns #-}\n\n"

genMain :: ExpEnv -> MonadicMap -> (Params, Expression) -> TypeScheme -> HaskellCode
genMain eenv m (params, startExp) t =  
  let (h, b) = evalState (translate m startExp) 0 -- in
      b1 = monadicStart eenv startExp in
  if b || b1 then
    "main = start >>= \\res -> putStrLn (show (res :: " ++ show t ++ "))\n\n"
  else
    "main = putStrLn (show start)\n\n"

monadicStart :: ExpEnv -> Expression -> Bool
monadicStart eenv (Variable p x) =
  if Map.member x eenv then monadicStart eenv (snd (eenv Map.! x)) else False
monadicStart eenv (UnLet p x e1 e2) = monadicStart eenv e1 || monadicStart eenv e2
monadicStart eenv (App p e1 e2) = monadicStart eenv e1 || monadicStart eenv e2
monadicStart eenv (TypeApp p e ts) = monadicStart eenv e-- TODO
monadicStart eenv (BinLet p x y e1 e2) = monadicStart eenv e1 || monadicStart eenv e2
monadicStart eenv (New p t) = True
monadicStart eenv (Send p e1 e2) = True
monadicStart eenv (Receive p e) = True
monadicStart eenv (Select p x e) = True
monadicStart eenv (Match p e mmap) = True
monadicStart eenv (Fork p e) = True
monadicStart eenv _ = False

-- TODO: Review
-- GEN DATATYPES

showDT :: Map.Map TypeVar [(TypeVar, TypeScheme)] -> HaskellCode
showDT m =
  Map.foldlWithKey' (\acc n dl ->
                       acc ++ "data " ++ n ++ " = " ++
                       intercalate " | " (showDTList dl) ++ " deriving Show\n") "" m
  where
    showDTList :: [(TypeVar, TypeScheme)] -> [HaskellCode]
    showDTList l =
      foldl (\acc (k, (TypeScheme _ v)) -> acc ++
              [k ++ " " ++ intercalate " " (init (showType v))]) [] l

    showType (Fun Un t1 t2) = showType t1 ++ [" "] ++ showType t2
    showType t = [show t]

genDataType :: Map.Map TypeVar TypeScheme -> Map.Map TypeVar Kind -> Map.Map TypeVar [(TypeVar, TypeScheme)]
genDataType cenv kenv =
  Map.foldlWithKey' (\acc k _ -> Map.insert k (fromCenv cenv k) acc) Map.empty kenv
  where
    fromCenv :: Map.Map TypeVar TypeScheme -> TypeVar -> [(TypeVar, TypeScheme)]
    fromCenv m c = Map.foldlWithKey' (checkLast c) [] m

    checkLast :: TypeVar -> [(TypeVar, TypeScheme)] -> TypeVar -> TypeScheme -> [(TypeVar, TypeScheme)]
    checkLast c acc k t
      | last (toList t) == (TypeScheme [] (Var c)) = acc ++ [(k, t)]
      | otherwise = acc
    

-- GENERATES THE COMMUNICATION AND THREAD CREATION MODULE

genCFSTComm :: FilePath -> IO ()
genCFSTComm path = do
  b <- doesFileExist (path ++ "CFSTCommunication.hs")
  if b then return () else genCFSTCommFile (path ++ "CFSTCommunication.hs")

genCFSTCommFile :: FilePath -> IO ()
genCFSTCommFile path =
  writeFile path ("module CFSTCommunication (fork, new, send, receive) where\n\n" ++
                  genCFSTCommImports ++ "\n\n" ++
                  genFork ++ "\n\n" ++ genNew ++ "\n\n" ++
                  genSend ++ "\n\n" ++ genReceive)

genCFSTCommImports :: String
genCFSTCommImports = "import Control.Concurrent (forkIO)\nimport Control.Concurrent.Chan.Synchronous\nimport Unsafe.Coerce"

genFork :: String
genFork = "fork e = do\n  forkIO e\n  return ()"

genNew :: String
genNew = "new = do\n  ch <- newChan\n  return (ch, ch)"

genSend :: String
genSend = "send x ch  = do\n  writeChan ch (unsafeCoerce x)\n  return ch"

genReceive :: String
genReceive = "receive ch = do\n  a <- readChan ch\n  return ((unsafeCoerce a), ch)"







  
-- TESTING
-- tester (test t2) t2
tester :: ExpEnv -> IO ()
tester eenv = putStrLn $ tester' (test eenv) eenv

tester' :: MonadicMap -> ExpEnv -> String
tester' m eenv =
  Map.foldrWithKey (\f (p, e) acc ->
                      acc ++ f ++ showParams p ++ " = " ++ code e ++ "\n\n") "" eenv
  where 
        code e = fst $ evalState (translate m e) 0

showParams :: Params -> String
showParams [] = ""
showParams args = " " ++ unwords args

test :: ExpEnv -> MonadicMap
test = isMonadicEnv

-- OK
-- let x = 2 in x
t0 = Map.fromList [("f1", ([], (UnLet (12,11) "val" (Integer (1,2) 2) (Variable (0,0) "val"))))]

-- OK
-- let x = 2+2 in x 
t1 = Map.fromList [("f1", ([], (UnLet (12,11) "val1" (App (0,0) (App (0,0) (Variable (0,0) "(+)") (Integer (0,0) 2)) (Integer (0,0) 2)) (Variable (0,0) "val1"))))]

-- OK
-- let x = send (2+2) c in x
t2 = Map.fromList [("f1", ([], (UnLet (12,11) "val1" (Send (12,20) (App (0,0) (App (0,0) (Variable (0,0) "(+)") (Integer (0,0) 2)) (Integer (0,0) 2)) (Variable (0,0) "c")) (Variable (0,0) "val1"))))]

-- OK
-- let x = send 5 c in x
t3 = Map.fromList [("f1", ([], (UnLet (12,11) "zzz" (Send (12,20) (Integer (1,2) 5) (Variable (0,0) "c"))) (Variable (0,0) "zzz")))]


-- Simple fork
-- let x = fork (client w) in x
t4 = Map.fromList [("client",(["x"],Variable (10,12) "x")),("f1",(["w"],UnLet (3,7) "x" (Fork (3,16) (App (3,17) (Variable (3,17) "client") (Variable (3,24) "w"))) (Variable (4,3) "x")))]

-- start = let w,r = (3,True) in r
t5 = Map.fromList [("start",([],BinLet (12,7) "w" "r" (Pair (12,14) (Integer (12,14) 3) (Boolean (12,16) True)) (Variable (13,3) "r")))]

-- OUT: start = new >>= \(w,r) -> fork ((boolServer r)) >>= \x -> return (client1 w)
t6 = Map.fromList [("start",([],BinLet (15,7) "w" "r" (New (15,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (16,7) "x" (Fork (16,16) (App (16,17) (Variable (16,17) "boolServer") (Variable (16,28) "r"))) (App (17,3) (Variable (17,3) "client") (Variable (17,11) "w")))))]

-- with client1
-- OUT: ... client1 w ....
t7 = Map.fromList [("client1",(["w"],UnLet (21,7) "w1" (Select (21,19) "And" (Variable (21,23) "w")) (UnLet (22,7) "w2" (Send (22,17) (Boolean (22,17) True) (Variable (22,22) "w1")) (UnLet (23,7) "r1" (Send (23,17) (Boolean (23,17) False) (Variable (23,23) "w2")) (BinLet (24,7) "x" "r2" (Receive (24,23) (Variable (24,23) "r1")) (Variable (25,3) "x")))))),("start",([],BinLet (15,7) "w" "r" (New (15,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (16,7) "x" (Fork (16,16) (App (16,17) (Variable (16,17) "boolServer") (Variable (16,28) "r"))) (App (17,3) (Variable (17,3) "client1") (Variable (17,11) "w")))))]

-- Boolserver original
t8 = Map.fromList [("boolServer",(["c"],Match (31,9) (Variable (31,9) "c") (Map.fromList [("And",("c1",BinLet (33,11) "n1" "c2" (Receive (33,28) (Variable (33,28) "c1")) (BinLet (34,11) "n2" "c3" (Receive (34,28) (Variable (34,28) "c2")) (UnLet (35,11) "x" (Send (35,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (35,21) "n1")) (Variable (35,27) "n2")) (Variable (35,31) "c3")) (Unit (36,7)))))),("Not",("c1",BinLet (45,11) "n1" "c2" (Receive (45,28) (Variable (45,28) "c1")) (UnLet (46,11) "x" (Send (46,20) (App (0,0) (Variable (0,0) "not") (Variable (46,25) "n1")) (Variable (46,29) "c2")) (Unit (47,7))))),("Or",("c1",BinLet (39,11) "n1" "c2" (Receive (39,28) (Variable (39,28) "c1")) (BinLet (40,11) "n2" "c3" (Receive (40,28) (Variable (40,28) "c2")) (UnLet (41,11) "x" (Send (41,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (41,21) "n1")) (Variable (41,27) "n2")) (Variable (41,31) "c3")) (Unit (42,7))))))]))),("client1",(["w"],UnLet (21,7) "w1" (Select (21,19) "And" (Variable (21,23) "w")) (UnLet (22,7) "w2" (Send (22,17) (Boolean (22,17) True) (Variable (22,22) "w1")) (UnLet (23,7) "r1" (Send (23,17) (Boolean (23,17) False) (Variable (23,23) "w2")) (BinLet (24,7) "x" "r2" (Receive (24,23) (Variable (24,23) "r1")) (Variable (25,3) "x")))))),("start",([],BinLet (15,7) "w" "r" (New (15,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (16,7) "x" (Fork (16,16) (App (16,17) (Variable (16,17) "boolServer") (Variable (16,28) "r"))) (App (17,3) (Variable (17,3) "client1") (Variable (17,11) "w")))))]

-- f1 c = send 5 c >>= \zzz -> return ()
t9 = Map.fromList [("f1", (["c"], (UnLet (12,11) "zzz" (Send (12,20) (Integer (1,2) 5) (Variable (0,0) "c"))) (Unit (21,0))))]

-- (Match (31,9) (Variable (31,9) "c") (fromList [("And",("c1",BinLet (33,11) "n1" "c2" (Receive (33,28) (Variable (33,28) "c1")) (BinLet (34,11) "n2" "c3" (Receive (34,28) (Variable (34,28) "c2")) (UnLet (35,11) "x" (Send (35,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (35,21) "n1")) (Variable (35,27) "n2")) (Variable (35,31) "c3")) (Unit (36,7)))))),("Not",("c1",BinLet (45,11) "n1" "c2" (Receive (45,28) (Variable (45,28) "c1")) (UnLet (46,11) "x" (Send (46,20) (App (0,0) (Variable (0,0) "not") (Variable (46,25) "n1")) (Variable (46,29) "c2")) (Unit (47,7))))),("Or",("c1",BinLet (39,11) "n1" "c2" (Receive (39,28) (Variable (39,28) "c1")) (BinLet (40,11) "n2" "c3" (Receive (40,28) (Variable (40,28) "c2")) (UnLet (41,11) "x" (Send (41,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (41,21) "n1")) (Variable (41,27) "n2")) (Variable (41,31) "c3")) (Unit (42,7))))))]),True)

t10 = Map.fromList [("boolServer",(["c"],Match (5,9) (Variable (5,9) "c") (Map.fromList [("And",("c1",BinLet (7,11) "n1" "c2" (Receive (7,28) (Variable (7,28) "c1")) (BinLet (8,11) "n2" "c3" (Receive (8,28) (Variable (8,28) "c2")) (UnLet (9,11) "x" (Send (9,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (9,21) "n1")) (Variable (9,27) "n2")) (Variable (9,31) "c3")) (Unit (10,7)))))),("Not",("c1",BinLet (19,11) "n1" "c2" (Receive (19,28) (Variable (19,28) "c1")) (UnLet (20,11) "x" (Send (20,20) (App (0,0) (Variable (0,0) "not") (Variable (20,25) "n1")) (Variable (20,29) "c2")) (Unit (21,7))))),("Or",("c1",BinLet (13,11) "n1" "c2" (Receive (13,28) (Variable (13,28) "c1")) (BinLet (14,11) "n2" "c3" (Receive (14,28) (Variable (14,28) "c2")) (UnLet (15,11) "x" (Send (15,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (15,21) "n1")) (Variable (15,27) "n2")) (Variable (15,31) "c3")) (Unit (16,7))))))]))),("client1",(["w"],UnLet (29,7) "w1" (Select (29,19) "And" (Variable (29,23) "w")) (UnLet (30,7) "w2" (Send (30,17) (Boolean (30,17) True) (Variable (30,22) "w1")) (UnLet (31,7) "r1" (Send (31,17) (Boolean (31,17) False) (Variable (31,23) "w2")) (BinLet (32,7) "x" "r2" (Receive (32,23) (Variable (32,23) "r1")) (Variable (33,3) "x")))))),("start",([],App (25,9) (Variable (25,9) "startClient") (Variable (25,21) "client1"))),("startClient",(["client"],BinLet (38,7) "w" "r" (New (38,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (39,7) "x" (Fork (39,16) (App (39,17) (Variable (39,17) "boolServer") (Variable (39,28) "r"))) (App (40,3) (Variable (40,3) "client") (Variable (40,10) "w")))))]
