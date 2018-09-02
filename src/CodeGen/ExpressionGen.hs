module CodeGen.ExpressionGen
 (
   HaskellCode(..)
 , MonadicMap
 , monadicFuns
 , isMonadic
 , translate
 ) where 


import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           Syntax.Terms


-- 1ST PASSAGE

-- After this passage each node of the AST is annotated
-- with a Boolean value that gives the information of the
-- monadic form of the node

type MonadicMap = Map.Map Expression Bool

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
      (m2, b2) = isMonadic fm False m1 e2 
--      (Map.insert (App p e1 e2) False m2, False)
      bool     = b && monadicVar fm e1 in
      (Map.insert (App p e1 e2) (bool || b1 || b2) m2, b1 || b2)

isMonadic fm b m (TypeApp p e ts) = 
  let (m1, b1) = isMonadic fm False m e in
      (Map.insert (TypeApp p e ts) (b || b1) m1, b || b1)

isMonadic fm b m (Conditional p e1 e2 e3) =
  let (m1, _) = isMonadic fm False m e1
      (m2, _) = isMonadic fm b m1 e2
      (m3, b1) = isMonadic fm b m2 e3 in
    (Map.insert (Conditional p e1 e2 e3) b1 m3, b1)
      
isMonadic fm b m (Pair p e1 e2)  = 
  let (m1, b1) = isMonadic fm False m e1
      (m2, b2) = isMonadic fm False m1 e2 in
      (Map.insert (Pair p e1 e2) (b || b1 || b2) m2, b || b1 || b2)
--      (Map.insert (Pair p e1 e2) b m2, b)

isMonadic fm b m (BinLet p x y e1 e2)  = 
  let (m1, b1) = isMonadic fm b m e1
      (m2, b2) = isMonadic fm b1 m1 e2 in
--      bool     = b && monadicVar fm e1 in
      (Map.insert (BinLet p x y e1 e2) b2 m2, b2)
--      (Map.insert (BinLet p x y e1 e2) (bool || b1 || b2) m2, b1 || b2)
      
isMonadic fm _ m (New p t) = (Map.insert (New p t) True m, True)

isMonadic fm _ m (Send p e1 e2) =
  let (m1, _) = isMonadic fm False m e1
      (m2, _) = isMonadic fm False m1 e2 in
      (Map.insert (Send p e1 e2) True m2, True)
      
isMonadic fm _ m (Receive p e) =  
  let (m1,_) = isMonadic fm False m e in
      (Map.insert (Receive p e) True m1, True)
      
isMonadic fm _ m (Select p x e) =  
  let (m1,_) = isMonadic fm False m e in
      (Map.insert (Select p x e) True m1, True)
      
isMonadic fm _ m (Match p e mmap) =
  let m1 = isMapMonadic True fm m mmap in
      (Map.insert (Match p e mmap) True m1, True)

isMonadic fm _ m (Fork p e) =
  let (m1,_) = isMonadic fm True m e in
  (Map.insert (Fork p e) True m1, True)

isMonadic fm b m (Constructor p x) = (Map.insert (Constructor p x) False m, False)

isMonadic fm b m (Case p e cm) = 
  let m1 = isMapMonadic b fm m cm in
      (Map.insert (Case p e cm) False m1, False) 

isMapMonadic :: Bool -> FunsMap -> MonadicMap -> Map.Map a (b, Expression) -> MonadicMap
isMapMonadic b fm m mmap = Map.foldr (\x acc -> fst $ isMonadic fm b acc (snd x)) m mmap

monadicVar :: FunsMap -> Expression -> Bool
monadicVar fm (Variable _ x)
  | Map.member x fm = fm Map.! x
  | otherwise = False
monadicVar _ _ = True

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
  | otherwise         = do
      f <- nextFresh
      return $ c ++ " >>= \\" ++ f ++ " -> " ++ f

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

  if b1 || b2 then
    return (h1 ++ " >>= \\" ++ x ++ " -> " ++ h2, True) 
  else
    return ("let " ++ x ++ " = " ++ h1 ++ " in " ++ h2, b2)
--  return (translateExpr (c1 ++ " " ++ c2) (expected m (UnLet p x e1 e2)) b2, b2)

translate fm m (App p e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2

  if (not b1) && b2 then
    do
      v <- nextFresh
      c <- translateExpr ("(" ++ h1 ++ " " ++ v ++ ")") (expected m (App p e1 e2)) False -- TODO
      return (h2 ++ " >>= \\ " ++ v ++ " -> " ++ c, b1)
  else
    do
      c <- translateExpr ("(" ++ h1 ++ " " ++ h2 ++ ")") (expected m (App p e1 e2)) (b1||b2)
      return (c, b1 || b2)

{- Was
translate fm m (App p e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2
  
  c <- translateExpr ("(" ++ h1 ++ " " ++ h2 ++ ")") (expected m (App p e1 e2)) (b1||b2)
  return (c, b1 || b2)
-}

translate fm m (TypeApp p e ts) = translate fm m e

translate fm m (Conditional _ c e1 e2) = do
  (b1, _) <- translate fm m c
  (h1, b2) <- translate fm m e1
  (h2, b3) <- translate fm m e2

  return ("if " ++ b1 ++ " then " ++ h1 ++ " else " ++ h2, b2 || b3)

translate fm m (Pair p e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2
  (hc1, hc2) <- genPair h1 b1 h2 b2
  c <- translateExpr hc2 (expected m (Pair p e1 e2)) False
  --c <- translateExpr ("(" ++ h1 ++ ", " ++ h2 ++ ")") (expected m (Pair p e1 e2)) (b1||b2)
  return (hc1 ++ c, False)

translate fm m (BinLet p x y e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2
  
  if b1 then
    return (h1  ++ " >>= \\(" ++ x ++ ", " ++ y ++ ")" ++ " -> " ++ h2, True) 
  else
    return ("let (" ++ x ++ ", " ++ y ++ ")" ++ " = " ++ h1 ++ " in " ++ h2, b2)
  
translate fm m (New _ _) = return ("_new", True)

translate fm m (Send p e1 e2) = do
  (h1, _) <- translate fm m e1
  (h2, _) <- translate fm m e2
  c <- translateExpr ("_send " ++ h1 ++ " " ++ h2) (expected m (Send p e1 e2)) True
  return (c, True)

translate fm m (Receive p e) = do
  (h, b) <- translate fm m e
  c <- translateExpr h (expected m e) b -- TRUE?

  if b then
    do
      v <- nextFresh
      return ("\\" ++ v ++ " -> " ++ "_receive " ++ c, True)
  else
    return ("_receive " ++ c, True)

translate fm m (Select p x e) = do
  (h, _) <- translate fm m e
  return ("_send \"" ++ x ++ "\" " ++ h, True)  

translate fm m (Match p e mm) = do
  (h1, b1) <- translate fm m e
  v <- nextFresh
  fresh <- nextFresh
  h2 <- translateMatchMap fresh fm m mm  
  return ("_receive " ++ h1 ++ " >>= \\(" ++ v ++  ", " ++ fresh ++
          ") -> case " ++ v ++ " of " ++ h2, False)

translate fm m (Fork p e) = do
  (h1, b1) <- translate fm m e
  c1 <- translateExpr ("_fork (" ++ h1 ++ " >> return ())") (expected m (Fork p e)) True
  return (c1, True)
  
translate fm m (Constructor p x) = do
  h <- translateExpr x (expected m (Constructor p x)) False
  return (h, False)

translate fm m (Case p e cm) = do
  (h1,_) <- translate fm m e 
  hcase <- translateCaseMap fm m cm
  return ("case " ++ h1 ++ " of " ++ hcase, False) -- TODO: Can be monadic
  
-- TODO: Join with case
translateMatchMap :: String -> FunsMap -> MonadicMap -> MatchMap -> TranslateMonad String
translateMatchMap fresh fm m = Map.foldlWithKey (translateMatchMap' fresh) (return "")
  where
    translateMatchMap' fresh acc v (param, e) = do
      (h, b) <- translate fm m e
      acc' <- acc
      return $ acc' ++ "\n    \"" ++ v ++ "\" " ++
        " -> let " ++ param ++ " = " ++ fresh ++ " in " ++ h
             

translateCaseMap :: FunsMap -> MonadicMap -> CaseMap -> TranslateMonad String
translateCaseMap fm m = Map.foldlWithKey translateCaseMap' (return "")
  where
    translateCaseMap' acc v (params, e) = do
      (h1, _) <- translate fm m e
      acc' <- acc
      return (acc' ++ "\n    " ++ v ++ showCaseParams params ++ " -> " ++ h1 ++ " ")


-- Gen pairs, if one of the elements is monadic extract it from the pair
-- bind it to a variable and return a pair that containt that variable
genPair :: HaskellCode -> Bool -> HaskellCode -> Bool -> TranslateMonad (HaskellCode, HaskellCode)
genPair h1 False h2 False = return $ ("", "(" ++ h1 ++ ", " ++ h2 ++ ")")
genPair h1 False h2 True =  do
  v <- nextFresh
  return (h2 ++ " >>= \\" ++ v ++ " -> ", "(" ++ h1 ++ ", " ++ v ++ ")")
genPair h1 True h2 False =  do
  v <- nextFresh
  return (h1 ++ " >>= \\" ++ v ++ " -> ", "(" ++ v ++ ", " ++ h2 ++ ")")
  
genPair h1 True h2 True =  do
  v1 <- nextFresh
  v2 <- nextFresh
  return (h1 ++ " >>= \\" ++ v1 ++ " -> " ++ h2 ++ " >>= \\" ++ v2 ++ " -> ",
          "(" ++ v1 ++ ", " ++ v2 ++ ")")
  

type FunsMap = Map.Map TermVar Bool
monadicFuns :: ExpEnv -> Map.Map TermVar Bool
monadicFuns eenv =
  Map.foldrWithKey (\f (_, e) acc ->
                     Map.insert f
                     (monadicFun eenv f e) acc) Map.empty eenv


monadicFun :: ExpEnv -> TermVar -> Expression -> Bool
monadicFun eenv fun (Variable p x)
  | Map.member x eenv && fun /= x = monadicFun eenv x (snd (eenv Map.! x))
  | otherwise                     = False
monadicFun eenv fun (UnLet _ _ e1 e2) = monadicFun eenv fun e1 || monadicFun eenv fun e2
monadicFun eenv fun (App _ e1 e2) = monadicFun eenv fun e1 || monadicFun eenv fun e2
monadicFun eenv fun (TypeApp _ e _) = monadicFun eenv fun e-- TODO?
monadicFun eenv fun (BinLet _ _ _ e1 e2) = monadicFun eenv fun e1 || monadicFun eenv fun e2
monadicFun _ _ (New _ _) = True
monadicFun _ _ (Send _ _ _) = True
monadicFun _ _ (Receive _ _) = True
monadicFun _ _ (Select _ _ _) = True
monadicFun _ _ (Match _ _ _) = True
monadicFun _ _ (Fork _ _) = True
monadicFun eenv fun (Case _ e cm) = monadicFun eenv fun e || monadicCase eenv fun cm
monadicFun _ _ _ = False

monadicCase :: ExpEnv -> TermVar -> CaseMap -> Bool
monadicCase eenv x = Map.foldr (\(_, e) acc -> acc || monadicFun eenv x e) False


showCaseParams :: Params -> String
showCaseParams [] = ""
showCaseParams args = " " ++ unwords args


