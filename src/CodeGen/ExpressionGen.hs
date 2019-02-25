module CodeGen.ExpressionGen
 (
   HaskellCode  
 , translateExpEnv
 , annotateAST
 , monadicFuns  -- TMP...
 , translate
 ) where 


import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           Syntax.Terms

{- 1st passage:
----------------
1) In this passage each node of the AST is annotated
with a Boolean value that represents if it should be on
IO form or not.

2) This passage also creates a map called FunsMap that
associates a Boolean value to each function name ginving
information if it is IO or not.
TODO: Adapt
-}

type FunsMap = Map.Map TermVar Bool

monadicFuns :: ExpEnv -> FunsMap
monadicFuns eenv =
  Map.foldrWithKey (\f (_, _, e) acc ->
                     Map.insert f
                     (monadicFun eenv f e) acc) Map.empty eenv
  where 
    monadicFun :: ExpEnv -> TermVar -> Expression -> Bool
    monadicFun eenv fun (Variable p x)
      | Map.member x eenv && fun /= x = let (_,_,e) = eenv Map.! x in monadicFun eenv x e
      | otherwise                     = False
    monadicFun eenv fun (UnLet _ _ e1 e2) = monadicFun eenv fun e1 || monadicFun eenv fun e2
    monadicFun eenv fun (App _ e1 e2) = monadicFun eenv fun e1 || monadicFun eenv fun e2
    monadicFun eenv fun (TypeApp _ e _) = monadicFun eenv fun e-- TODO?
    monadicFun eenv fun (Conditional _ e1 e2 e3) = 
      monadicFun eenv fun e1 || monadicFun eenv fun e2 || monadicFun eenv fun e3
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


type MonadicMap = Map.Map Expression Bool

-- annotateAST :: ExpEnv -> (FunsMap, MonadicMap)
-- annotateAST eenv =
--   let m = monadicFuns eenv in 
--   (m,Map.foldrWithKey (\f (_,_,e) acc -> fst $ annotateAST' acc m (m Map.! f) e) Map.empty eenv)

annotateAST :: FunsMap -> String -> Expression -> MonadicMap
annotateAST fm f e = fst $ annotateAST' Map.empty fm (fm Map.! f) e

annotateAST' :: MonadicMap -> FunsMap -> Bool -> Expression -> (MonadicMap, Bool)
annotateAST' m fm b e@(Unit _)        = (Map.insert e b m, b)
annotateAST' m fm b e@(Integer _ _)   = (Map.insert e b m, b)
annotateAST' m fm b e@(Character _ _) = (Map.insert e b m, b)
annotateAST' m fm b e@(Boolean _ _)   = (Map.insert e b m, b)
annotateAST' m fm b e@(Variable _ x)  =
  case fm Map.!? x of
    Just b1 -> (Map.insert e b1 m, b1)
    Nothing -> (Map.insert e b m, b)
    
annotateAST' m fm b e@(UnLet _ x e1 e2)  = -- TODO: Check return type boolean 
  let (m1, b1) = annotateAST' m fm b e1
      (m2, b2) = annotateAST' m1 fm b1 e2 in
      (Map.insert e b2 m2, b2) -- was b2?
  
annotateAST' m fm b e@(App _ e1 e2) = 
  let (m1, b1) = annotateAST' m fm False e1 -- TODO: False??
      (m2, b2) = annotateAST' m1 fm False e2 
      b3 = b && monadicVar fm e1 in
      (Map.insert e (b1 || b2 || b3) m2, b1 || b2)
 
annotateAST' m fm b e@(TypeApp _ e1 _) = -- TODO: Was: b = False
  let (m1, b1) = annotateAST' m fm False e1 in
     -- b1 = b && monadicVar fm e1 in
      (Map.insert e (b || b1) m1, b || b1)

annotateAST' m fm b e@(Conditional _ e1 e2 e3) =
  let (m1,_) = annotateAST' m fm False  e1
      (m2,_) = annotateAST' m1 fm b e2
      (m3,b1) = annotateAST' m2 fm b e3 in      
      (Map.insert e b1 m3, b1)

annotateAST' m fm b e@(Pair _ e1 e2)  = 
  let (m1,b1) = annotateAST' m fm False e1
      (m2,b2) = annotateAST' m1 fm False e2
      b3      = b || b1 || b2 in
      (Map.insert e b3 m2, b3)

annotateAST' m fm b e@(BinLet _ _ _ e1 e2)  = 
  let (m1,b1) = annotateAST' m fm b e1
      (m2,b2) = annotateAST' m1 fm b1 e2 in
      (Map.insert e b2 m2, b2)
      
annotateAST' m fm _ e@(New _ _) = (Map.insert e True m, True)


annotateAST' m fm _ e@(Send _ e1 e2) =
  let (m1, _) = annotateAST' m fm False e1
      (m2, _) = annotateAST' m1 fm False e2 in
      (Map.insert e True m2, True)
      
annotateAST' m fm _ e@(Receive _ e1) =  
  let (m1,_) = annotateAST' m fm False e1 in
      (Map.insert e True m1, True)
      
annotateAST' m fm _ e@(Select _ _ e1) =  
  let (m1,_) = annotateAST' m fm False e1 in
      (Map.insert e True m1, True)
      
annotateAST' m fm _ e@(Match _ _ mmap) =
  let m1 = annotateMap m fm True mmap in
      (Map.insert e True m1, True)

annotateAST' m fm _ e@(Fork _ e1) =
  let (m1,_) = annotateAST' m fm True e1 in
      (Map.insert e True m1, True)

annotateAST' m fm b e@(Constructor _ _) = (Map.insert e b m, b)

annotateAST' m fm b e@(Case _ _ cm) =  -- TODO: False
  let m1 = annotateMap m fm b cm in
      (Map.insert e False m1, False)

annotateMap :: MonadicMap -> FunsMap -> Bool -> Map.Map a (b, Expression) -> MonadicMap
annotateMap m fm b = Map.foldr (\x acc -> fst $ annotateAST' acc fm b (snd x)) m

monadicVar :: FunsMap -> Expression -> Bool
monadicVar fm (Variable _ x) =
  case fm Map.!? x of
    Just x  -> x
    Nothing -> False
monadicVar _ _ = True


translateExpEnv :: ExpEnv -> HaskellCode
translateExpEnv eenv =
  let fm  = monadicFuns eenv in
  --  error $ show fm ++ "\n\n" ++ show mm
  Map.foldrWithKey (\f (_,ps,e) acc -> acc ++ genFun fm f ps e ++ "\n\n") "" eenv

  where
    genFun :: FunsMap -> String -> Params -> Expression -> HaskellCode
    genFun fm f ps e =
      let mm = annotateAST fm f e in
       f ++ showBangParams ps ++ " = " ++
        (fst (evalState (translate fm mm e) 0))

    showBangParams :: Params -> String
    showBangParams [] = ""
    showBangParams args = " !" ++ intercalate " !" args


    
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


translate :: FunsMap -> MonadicMap -> Expression -> TranslateMonad (HaskellCode, Bool)
translate fm m e@(Unit _) = do
  let b = expected m e
  h <- translateExpr "()" b False
  return (h, b)
  
translate fm m e@(Integer _ i) = do
  let b = expected m e
  h <- translateExpr (show i) b False
  return (h, b)
  
translate fm m e@(Character _ c) = do
  let b = expected m e
  h <- translateExpr (show c) b False
  return (h, b)
  
translate fm m e@(Boolean _ b) = do
  let b1 = expected m e
  h <- translateExpr (show b) b1 False
  return (h, b1)

translate fm m e@(Variable _ x) = do
  let b = expected m e
  if Map.member x fm then
    do      
      h <- translateExpr x b (fm Map.! x)
      return (h, b) -- fm Map.! x)
  else
    do
      h <- translateExpr x b False
      return (h, b)
  
translate fm m e@(UnLet _ (_,x) e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2

  if b1 || b2 then
    return (h1 ++ " >>= \\" ++ x ++ " -> " ++ h2, True) 
  else
    return ("let " ++ x ++ " = " ++ h1 ++ " in " ++ h2, b2)

translate fm m e@(App _ e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2

  if (not b1) && b2 then
    do
      v <- nextFresh
      c <- translateExpr ("(" ++ h1 ++ " " ++ v ++ ")") (expected m e) False
      return (h2 ++ " >>= \\ " ++ v ++ " -> " ++ c, b1)
  else
    do
      c <- translateExpr ("(" ++ h1 ++ " " ++ h2 ++ ")") (expected m e) (b1||b2) --False
      return (c, b1 || b2)

translate fm m (TypeApp _ e _) = translate fm m e

translate fm m (Conditional _ c e1 e2) = do
  (b1, _) <- translate fm m c
  (h1, b2) <- translate fm m e1
  (h2, b3) <- translate fm m e2

  return ("if " ++ b1 ++ " then " ++ h1 ++ " else " ++ h2, b2 || b3)

translate fm m e@(Pair _ e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2
  (hc1, hc2) <- genPair h1 b1 h2 b2
  c <- translateExpr hc2 (expected m e) False
  return (hc1 ++ c, False)

translate fm m (BinLet _ (_,x) (_,y) e1 e2) = do
  (h1, b1) <- translate fm m e1
  (h2, b2) <- translate fm m e2
  
  if b1 then
    return (h1  ++ " >>= \\(" ++ x ++ ", " ++ y ++ ")" ++ " -> " ++ h2, True) 
  else
    return ("let (" ++ x ++ ", " ++ y ++ ")" ++ " = " ++ h1 ++ " in " ++ h2, b2)
  
translate fm m (New _ _) = return ("_new", True)

translate fm m e@(Send _ e1 e2) = do
  (h1, _) <- translate fm m e1
  (h2, _) <- translate fm m e2
  c <- translateExpr ("(_send " ++ h1 ++ " " ++ h2 ++ ")") (expected m e) True
  return (c, True)

translate fm m (Receive _ e) = do
  (h, b) <- translate fm m e
  c <- translateExpr h (expected m e) b -- TRUE?

  if b then
    do
      v <- nextFresh
      return ("\\" ++ v ++ " -> " ++ "_receive " ++ c, True)
  else
    return ("_receive " ++ c, True)

translate fm m (Select _ x e) = do
  (h, _) <- translate fm m e
  return ("_send \"" ++ x ++ "\" " ++ h, True)  

translate fm m (Match _ e mm) = do
  (h1, b1) <- translate fm m e
  v <- nextFresh
  fresh <- nextFresh
  h2 <- translateMatchMap fresh fm m mm  
  return ("_receive " ++ h1 ++ " >>= \\(" ++ v ++  ", " ++ fresh ++
          ") -> (case " ++ v ++ " of {" ++ h2 ++ "})", False)

translate fm m e@(Fork _ e1) = do
  (h1, b1) <- translate fm m e1
  c1 <- translateExpr ("_fork (" ++ h1 ++ " >> return ())") (expected m e) True
  return (c1, True)
  
translate fm m e@(Constructor _ x) = do
  h <- translateExpr x (expected m e) False
  return (h, False)

translate fm m (Case _ e cm) = do
  (h1,_) <- translate fm m e 
  hcase <- translateCaseMap fm m cm
  return ("case " ++ h1 ++ " of {" ++ hcase ++ "}", False) -- TODO: Can be monadic
  
-- TODO: Join with case
translateMatchMap :: String -> FunsMap -> MonadicMap -> MatchMap -> TranslateMonad String
translateMatchMap fresh fm m = Map.foldlWithKey (translateMatchMap' fresh) (return "")
  where
    translateMatchMap' fresh acc v (param, e) = do
      (h, b) <- translate fm m e
      acc' <- acc
      return $ acc' ++ "\n    \"" ++ v ++ "\" " ++
        " -> let " ++ param ++ " = " ++ fresh ++ " in " ++ h ++ ";"
             

translateCaseMap :: FunsMap -> MonadicMap -> CaseMap -> TranslateMonad String
translateCaseMap fm m = Map.foldlWithKey translateCaseMap' (return "")
  where
    translateCaseMap' acc v (params, e) = do
      (h1, _) <- translate fm m e
      acc' <- acc
      return (acc' ++ "\n    " ++ v ++ showCaseParams params ++ " -> " ++ h1 ++ ";")

    showCaseParams :: Params -> String
    showCaseParams [] = ""
    showCaseParams args = " " ++ unwords args

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
