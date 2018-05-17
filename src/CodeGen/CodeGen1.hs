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


type FunsMap = Map.Map TermVar Bool
monadicFuns :: ExpEnv -> Map.Map TermVar Bool
monadicFuns eenv =
  Map.foldrWithKey (\f (_, e) acc ->
                     Map.insert f
                     (monadicFun eenv e) acc) Map.empty eenv

  
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
     menv acc (p,x) = Map.union (fst (isMonadic Map.empty False acc x)) (monadicParams p)
     
     monadicParams :: Params -> MonadicMap
     monadicParams = foldr (\x acc -> fst $ (isMonadic Map.empty False acc (Variable (0,0) x))) Map.empty

-- This function checks if an expression is in the monadic form
-- and then updates the monadic map with that information.

isMonadic :: FunsMap ->  Bool -> MonadicMap -> Expression -> (MonadicMap, Bool)
isMonadic fm b m (Unit p) = (Map.insert (Unit p) b m, b)
isMonadic fm b m (Integer p i) = (Map.insert (Integer p i) b m, b)
isMonadic fm b m (Character p c) = (Map.insert (Character p c) b m, b)
isMonadic fm b' m (Boolean p b) = (Map.insert (Boolean p b) b' m, b')
isMonadic fm b m (Variable p x) =
  if Map.member x fm then
    let bf = fm Map.! x in
    (Map.insert (Variable p x) bf m, bf)
  else
    (Map.insert (Variable p x) b m, b)
isMonadic fm b m (UnLet p x e1 e2) =
  let (m1, b1) = isMonadic fm b m e1
      (m2, b2) = isMonadic fm b1 m1 e2 in
--      (Map.insert (UnLet p x e1 e2) b2 m2, b2)
--      m3 = Map.insert (Variable (27,3) x) (b1 || b2) m2 in
      (Map.insert (UnLet p x e1 e2) b2 m2, b2)
     
isMonadic fm b m (App p e1 e2) = 
  let (m1, b1) = isMonadic fm False m e1
      (m2, b2) = isMonadic fm False m1 e2 in
--      (Map.insert (App p e1 e2) False m2, False)
      (Map.insert (App p e1 e2) (b || b1 || b2) m2, b1 || b2)

isMonadic fm b m (TypeApp p e ts) = 
  let (m1, b1) = isMonadic fm b m e in
      (Map.insert (TypeApp p e ts) b1 m1, b1)

isMonadic fm b m (Conditional p e1 e2 e3) =
  let (m1, _) = isMonadic fm False m e1
      (m2, _) = isMonadic fm b m1 e2
      (m3, b1) = isMonadic fm b m2 e3 in
    (Map.insert (Conditional p e1 e2 e3) b1 m3, b1)
      
isMonadic fm b m (Pair p e1 e2)  = 
  let (m1, _) = isMonadic fm False m e1
      (m2, _) = isMonadic fm False m1 e2 in
      (Map.insert (Pair p e1 e2) b m2, b)

isMonadic fm b m (BinLet p x y e1 e2)  = 
  let (m1, b1) = isMonadic fm b m e1
      (m2, b2) = isMonadic fm b1 m1 e2 in
      (Map.insert (BinLet p x y e1 e2) b2 m2, b2)
      
isMonadic fm b m (New p t) = (Map.insert (New p t) True m, True)

isMonadic fm b m (Send p e1 e2) =
  let (m1, _) = isMonadic fm False m e1
      (m2, _) = isMonadic fm False m1 e2 in
      (Map.insert (Send p e1 e2) True m2, True)
      
isMonadic fm b m (Receive p e) =  
  let (m1,_) = isMonadic fm False m e in
      (Map.insert (Receive p e) True m1, True)
      
isMonadic fm b m (Select p x e) =  
  let (m1,_) = isMonadic fm False m e in
      (Map.insert (Select p x e) True m1, True)
      
isMonadic fm b m (Match p e mmap) =
  let m1 = isMapMonadic fm m mmap in
      (Map.insert (Match p e mmap) True m1, True)

isMonadic fm b m (Fork p e) =
  let (m1,_) = isMonadic fm True m e in
  (Map.insert (Fork p e) True m1, True)

isMonadic fm b m (Constructor p x) = (Map.insert (Constructor p x) False m, False)

isMonadic fm b m (Case p e cm) = 
  let m1 = isMapMonadic fm m cm in
      (Map.insert (Case p e cm) False m1, False) 

isMapMonadic :: FunsMap -> MonadicMap -> Map.Map a (b, Expression) -> MonadicMap
isMapMonadic fm m mmap = Map.foldr (\x acc -> fst $ isMonadic fm True acc (snd x)) m mmap

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

translate :: FunsMap -> MonadicMap -> Expression -> TranslateMonad (HaskellCode, Bool)
translate fm m (Unit p) = do
  let b = expected m (Unit p)
  h <- translateExpr "()" b False
  return (h, b)
  
translate fm m (Integer p i) = do
  let b = expected m (Integer p i)
  h <- translateExpr (show i) b False
  return (h, b)
  
translate fm m (Character p c) = do
  let b = expected m (Character p c)
  h <- translateExpr (show c) b False
  return (h, b)
  
translate fm m (Boolean p b) = do
  let b1 = expected m (Boolean p b)
  h <- translateExpr (show b) b1 False
  return (h, b1)

translate fm m (Variable p x) = do
  if Map.member x fm then
    do
      let b = expected m (Variable p x)
      h <- translateExpr x b (fm Map.! x)
      return (h, b) -- fm Map.! x)
  else
    do
      let b = expected m (Variable p x)
      h <- translateExpr x b False
      return (h, b)
  
translate fm m (UnLet p x e1 e2) = do 
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2

  -- c1 <- translateExpr h1 (expected m e1) b1
  -- c2 <- translateExpr h2 (expected m e2) b2
--  c2 <- translateExpr h2 (expected m (UnLet p x e1 e2)) b2

  if b1 || b2 then
    return (h1 ++ " >>= \\" ++ x ++ " -> " ++ h2, True) 
  else
    return ("let " ++ x ++ " = " ++ h1 ++ " in " ++ h2, b2)
--  return (translateExpr (c1 ++ " " ++ c2) (expected m (UnLet p x e1 e2)) b2, b2)

translate fm m (App p e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2

  --checkFun m (h1,b1) (h2,b2) e1 e2
-- (expected m (App p e1 e2))
  c <- translateExpr ("(" ++ h1 ++ " " ++ h2 ++ ")") (expected m (App p e1 e2)) (b1||b2)
  return (c, b1 || b2)

-- TODO:
translate fm m (TypeApp p e ts) = translate fm m e

translate fm m (Conditional _ c e1 e2) = do
  (b1, _) <- translate fm m c
  (h1, b2) <- translate fm m e1
  (h2, b3) <- translate fm m e2

--  c1 <- translateExpr h1 (expected m e1) b2
--  c2 <- translateExpr h2 (expected m e2) b3
  
  return ("if " ++ b1 ++ " then " ++ h1 ++ " else " ++ h2, b2 || b3)

translate fm m (Pair p e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2
  c <- translateExpr ("(" ++ h1 ++ ", " ++ h2 ++ ")") (expected m (Pair p e1 e2)) (b1||b2)
  return (c, False)

translate fm m (BinLet p x y e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2

 -- c1 <- translateExpr h1 (expected m e1) b1
 -- c2 <- translateExpr h2 (expected m (BinLet p x y e1 e2)) b2
--  c2 <- translateExpr h2 m e2 b2
  -- c1 <- translateExpr h1 (expected m e1) b1
  -- c2 <- translateExpr h2 (expected m e2) b2
  
  if b1 then
    return (h1  ++ " >>= \\(" ++ x ++ ", " ++ y ++ ")" ++ " -> " ++ h2, True) 
  else
    return ("let (" ++ x ++ ", " ++ y ++ ")" ++ " = " ++ h1 ++ " in " ++ h2, b2)
  
translate fm m (New _ _) = return ("new", True)

translate fm m (Send p e1 e2) = do
  (h1, _) <- translate fm m e1
  (h2, _) <- translate fm m e2
  c <- translateExpr ("send " ++ h1 ++ " " ++ h2) (expected m (Send p e1 e2)) True
  return (c, True)
--  return ("send " ++ h1 ++ " " ++ h2, True)

translate fm m (Receive p e) = do
  (h, b) <- translate fm m e
  c <- translateExpr h (expected m e) b -- TRUE?

  if b then
    do
      v <- nextFresh
      return ("\\" ++ v ++ " -> " ++ "receive " ++ c, True)
  else
    return ("receive " ++ c, True)

translate fm m (Select p x e) = do
  (h, _) <- translate fm m e
  -- c <- translateExpr ("send \"" ++ x ++ "\" " ++ h) (expected m (Select p x e)) True
  return ("send \"" ++ x ++ "\" " ++ h, True)  

translate fm m (Match p e mm) = do
  (h1, b1) <- translate fm m e
  (h2, params) <- translateMatchMap fm m mm
  v <- nextFresh
  return ("receive " ++ h1 ++ " >>= \\(" ++ v ++  ", " ++ (head params) ++
          ") -> case " ++ v ++ " of " ++ h2, False)

translate fm m (Fork p e) = do
  (h1, b1) <- translate fm m e
  c1 <- translateExpr ("fork (" ++ h1 ++ ")") (expected m (Fork p e)) True
  return (c1, True)
  
translate fm m (Constructor p x) = do
  h <- translateExpr x (expected m (Constructor p x)) False
  return (h, False)

translate fm m (Case p e cm) = do
  (h1,_) <- translate fm m e 
  hcase <- translateCaseMap fm m cm
  return ("case " ++ h1 ++ " of " ++ hcase, False) -- TODO: Can be monadic
  
-- TODO: Join with case
translateMatchMap :: FunsMap -> MonadicMap -> MatchMap -> TranslateMonad (String, [String])
translateMatchMap fm m = Map.foldlWithKey translateMatchMap' (return ("", []))
  where
    translateMatchMap' acc v (param, e) = do
      (h, b) <- translate fm m e
      acc' <- acc
      return (fst acc' ++ "\n    \"" ++ v ++ "\" " ++ " -> " ++ h
             , snd acc' ++ [param])

translateCaseMap :: FunsMap -> MonadicMap -> CaseMap -> TranslateMonad String
translateCaseMap fm m = Map.foldlWithKey translateCaseMap' (return "")
  where
    translateCaseMap' acc v (params, e) = do
      (h1, _) <- translate fm m e
      acc' <- acc
      return (acc' ++ "\n    " ++ v ++ showParams params ++ " -> " ++ h1 ++ " ")

-- TODO : PARAM BANG
genProgram :: VarEnv -> ExpEnv -> ConstructorEnv -> KindEnv -> FilePath -> IO ()
genProgram venv eenv cenv kenv path = do
  genCFSTComm path
  let start      = eenv Map.! "start"
      startType  = last $ toList $ venv Map.! "start"
      dataTypes  = showDT (genDataType cenv kenv)
      file       = genFile eenv
      mainFun    = genMain eenv start startType in
      writeFile (path ++ "cfst.hs") (genPragmas ++ genImports ++ dataTypes ++ file ++ mainFun)

expApp :: MonadicMap -> Expression -> Bool
expApp m e
  | Map.member e m = m Map.! e
  | otherwise      = True
  


genFile :: ExpEnv -> HaskellCode
genFile eenv =
  Map.foldrWithKey (\f (p, e) acc ->
                      acc ++ f ++ showParams p ++ " = " ++
                      code e ++ "\n\n") "" eenv
  where 
    code e =
      let m = monadicFuns eenv
          m1 = fst $ isMonadic m False Map.empty e in
      fst $ evalState (translate m m1 e) 0 




genImports :: String
genImports = "import CFSTCommunication\n\n"

genPragmas :: String
genPragmas = "{-# LANGUAGE BangPatterns #-}\n\n"

genMain :: ExpEnv  -> (Params, Expression) -> TypeScheme -> HaskellCode
genMain eenv (params, startExp) t =  
  let m = isMonadicEnv eenv 
      (h, b) = evalState (translate Map.empty m startExp) 0 -- in
      b1 = monadicFun eenv startExp in -- TODO: remove
  if b || b1 then
    "main = start >>= \\res -> putStrLn (show (res :: " ++ show t ++ "))\n\n"
  else
    "main = putStrLn (show start)\n\n"

monadicFun :: ExpEnv -> Expression -> Bool
monadicFun eenv (Variable p x) =
  if Map.member x eenv then monadicFun eenv (snd (eenv Map.! x)) else False
monadicFun eenv (UnLet _ _ e1 e2) = monadicFun eenv e1 || monadicFun eenv e2
monadicFun eenv (App _ e1 e2) = monadicFun eenv e1 || monadicFun eenv e2
monadicFun eenv (TypeApp _ e _) = monadicFun eenv e-- TODO?
monadicFun eenv (BinLet _ _ _ e1 e2) = monadicFun eenv e1 || monadicFun eenv e2
monadicFun eenv (New _ _) = True
monadicFun eenv (Send _ _ _) = True
monadicFun eenv (Receive _ _) = True
monadicFun eenv (Select _ _ _) = True
monadicFun eenv (Match _ _ _) = True
monadicFun eenv (Case _ e cm) = monadicFun eenv e || monadicCase eenv cm
monadicFun eenv (Fork _ _) = True
monadicFun eenv _ = False


monadicCase :: ExpEnv -> CaseMap -> Bool
monadicCase eenv = Map.foldr (\(_, e) acc -> acc || monadicFun eenv e) False

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

t str =
  let m0 = monadicFuns t13
    --  m1 = foldr (\x acc -> Map.insert x False acc) m0 (fst (t11 Map.! "start"))
      m2 = fst $ isMonadic m0 False Map.empty (snd (t13 Map.! str)) in
      fst $ evalState (translate m0 m2 (snd (t13 Map.! str))) 0

t' srt= let m = monadicFuns t11 in fst $ isMonadic m False Map.empty (snd (t13 Map.! srt))

tester :: ExpEnv -> IO ()
tester eenv = putStrLn $ tester' eenv

tester' :: ExpEnv -> String
tester' eenv =
  Map.foldrWithKey (\f (p, e) acc ->
                      acc ++ f ++ showParams p ++ " = " ++ code e p ++ "\n\n") "" eenv
  where 
    code e p =
      let m0 = monadicFuns eenv
          m1 = foldr (\x acc -> Map.insert x False acc) m0 p
          m2 = fst $ isMonadic m1 False Map.empty e in
      fst $ evalState (translate m1 m2 e) 0 

showParams :: Params -> String
showParams [] = ""
showParams args = " " ++ unwords args

test :: ExpEnv -> MonadicMap
test = isMonadicEnv

-- OK
-- let x = 2 in x
t0 = Map.fromList [("f1", ([], (UnLet (12,11) "val" (Integer (1,2) 2) (Variable (0,0) "val"))))]

   

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


t11 = Map.fromList [("boolServer",(["c"],Match (5,9) (Variable (5,9) "c") (Map.fromList [("And",("c1",BinLet (7,11) "n1" "c2" (Receive (7,28) (Variable (7,28) "c1")) (BinLet (8,11) "n2" "c3" (Receive (8,28) (Variable (8,28) "c2")) (UnLet (9,11) "x" (Send (9,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (9,21) "n1")) (Variable (9,27) "n2")) (Variable (9,31) "c3")) (Unit (10,7)))))),("Not",("c1",BinLet (19,11) "n1" "c2" (Receive (19,28) (Variable (19,28) "c1")) (UnLet (20,11) "x" (Send (20,20) (App (0,0) (Variable (0,0) "not") (Variable (20,25) "n1")) (Variable (20,29) "c2")) (Unit (21,7))))),("Or",("c1",BinLet (13,11) "n1" "c2" (Receive (13,28) (Variable (13,28) "c1")) (BinLet (14,11) "n2" "c3" (Receive (14,28) (Variable (14,28) "c2")) (UnLet (15,11) "x" (Send (15,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (15,21) "n1")) (Variable (15,27) "n2")) (Variable (15,31) "c3")) (Unit (16,7))))))]))),("client1",(["w"],UnLet (32,7) "w1" (Select (32,19) "And" (Variable (32,23) "w")) (UnLet (33,7) "w2" (Send (33,17) (Boolean (33,17) True) (Variable (33,22) "w1")) (UnLet (34,7) "r1" (Send (34,17) (Boolean (34,17) False) (Variable (34,23) "w2")) (BinLet (35,7) "x" "r2" (Receive (35,23) (Variable (35,23) "r1")) (Variable (36,3) "x")))))),("client2",(["w"],UnLet (40,7) "w1" (Select (40,19) "Not" (Variable (40,23) "w")) (UnLet (41,7) "r1" (Send (41,17) (Boolean (41,17) True) (Variable (41,22) "w1")) (BinLet (42,7) "x" "r2" (Receive (42,23) (Variable (42,23) "r1")) (Variable (43,3) "x"))))),("start",([],UnLet (25,7) "c1" (App (25,12) (Variable (25,12) "startClient") (Variable (25,24) "client1")) (UnLet (26,7) "c2" (App (26,12) (Variable (26,12) "startClient") (Variable (26,24) "client2")) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (27,3) "c1")) (Variable (27,9) "c2"))))),("startClient",(["client"],BinLet (48,7) "w" "r" (New (48,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (49,7) "x" (Fork (49,16) (App (49,17) (Variable (49,17) "boolServer") (Variable (49,28) "r"))) (App (50,3) (Variable (50,3) "client") (Variable (50,10) "w")))))]

t12 = Map.fromList [("sendTree",(["t","c"],Case (13,8) (Variable (13,8) "t") (Map.fromList [("Leaf",([],UnLet (15,11) "x" (Select (15,22) "LeafC" (Variable (15,28) "c")) (Unit (16,7)))),("Node",(["x","l","r"],UnLet (18,12) "x" (Select (18,23) "LeafC" (Variable (18,29) "c")) (Unit (19,7))))]))),("start",([],Integer (70,9) 10))]

t13 = Map.fromList [("receiveOne",(["c"],Match (40,9) (Variable (40,9) "c") (Map.fromList [("LeafC",("c1",Pair (42,8) (Constructor (42,13) "LeafA") (Constructor (42,20) "LeafA"))),("NodeC",("c1",BinLet (44,11) "x" "c2" (Receive (44,27) (Variable (44,27) "c1")) (BinLet (45,11) "left" "c3" (App (45,22) (Variable (45,22) "receiveOne") (Variable (45,33) "c2")) (Pair (47,8) (App (47,8) (App (47,8) (Constructor (47,8) "NodeA") (Variable (47,14) "x")) (Variable (47,16) "left")) (Variable (47,22) "left")))))]))),("sendOne",(["t","c"],Case (27,8) (Variable (27,8) "t") (Map.fromList [("LeafA",([],UnLet (29,11) "x" (Select (29,22) "LeafC" (Variable (29,28) "c")) (Unit (30,7)))),("NodeA",(["x","l"],UnLet (32,11) "w1" (Select (32,23) "NodeC" (Variable (32,29) "c")) (UnLet (33,11) "w2" (Send (33,21) (Variable (33,21) "x") (Variable (33,23) "w1")) (UnLet (34,11) "w3" (App (34,16) (App (34,16) (Variable (34,16) "sendOne") (Variable (34,24) "l")) (Variable (34,26) "w2")) (Unit (36,7))))))]))),("start",([],UnLet (113,6) "inTree" (App (113,15) (App (113,15) (Constructor (113,15) "NodeA") (Integer (113,21) 7)) (Constructor (113,29) "LeafA")) (BinLet (116,6) "writer" "reader" (New (116,26) (Rec (Bind {var = "x", kind = Kind {prekind = Session, multiplicity = Un}}) (Choice Internal (Map.fromList [("LeafC",Skip),("NodeC",Semi (Message Out IntType) (Var "x"))])))) (UnLet (117,6) "w" (Fork (117,15) (App (117,16) (App (117,16) (Variable (117,16) "sendOne") (Variable (117,24) "inTree")) (Variable (117,31) "writer"))) (BinLet (118,6) "outTree" "r" (App (118,19) (Variable (118,19) "receiveOne") (Variable (118,30) "reader")) (Variable (119,2) "outTree"))))))]
