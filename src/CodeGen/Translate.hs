{-# LANGUAGE MultiWayIf #-}
module CodeGen.Translate where

import           Syntax.Base
import           Syntax.Expressions
import           Syntax.ProgramVariables
import           Syntax.Schemes
import           Syntax.ProgramVariables
import           CodeGen.CodeGenState
import           CodeGen.Annotation
import           Utils.FreestState (tMapWithKeyM)
import           Control.Monad.State
import qualified Data.Map.Strict as Map

isIO :: NodeType -> Bool
isIO (ArrowType _ s2) = isIO s2
isIO IOType           = True
isIO _                = False

-- Creates a new function based on the types
-- createNewFun :: Function name -> function to call ->
--                 [(Expected type, actual type)] -> haskell code
createNewFun :: String -> ProgVar -> [(NodeType, NodeType)] -> TranslateM ()
createNewFun s x xs = do
  m <- getGenFunsMap
  if s `Map.member` m
    then return ()
    else do
      params <- sequence $ take (length xs-1) (repeat (nextFresh))
      c <- genFunCode x xs params []
      let ps = foldl (\acc a -> acc ++ "\\" ++ a ++ " -> ") "" params
      addFun s (ps ++ c)

genFunCode :: ProgVar -> [(NodeType, NodeType)] -> [String] -> [String] -> TranslateM HaskellCode
genFunCode pv ((x,y):[]) _ ys
  | x /= y = return $ "return (" ++ foldl (\acc a -> acc ++ " " ++ a) (show pv) ys ++ ")"
  | otherwise = return $ foldl (\acc a -> acc ++ " " ++ a) (show pv) ys
genFunCode pv ((x1,x2):xs) (y:ys) acc
  | x1 /= x2 = do
      f <- nextFresh   
      let c1 = y ++ " >>= \\" ++ f ++ " -> "
      c2 <- genFunCode pv xs ys (acc ++ [f])
      return $ c1 ++ c2
  | otherwise = genFunCode pv xs ys (acc ++ [y])
genFunCode pv xs ys acc = error $ show acc

zipNodeTypes :: NodeType -> NodeType -> [(NodeType, NodeType)]
zipNodeTypes (ArrowType s1 s2) (ArrowType s3 s4) = (s1, s3) : zipNodeTypes s2 s4
zipNodeTypes t1 t2 = [(t1, t2)]

showGenFunMap :: GenFunsMap -> String
showGenFunMap = Map.foldlWithKey (\acc h b -> acc ++ "\n\n" ++ h ++ " = " ++ b) ""

funName :: ProgVar -> NodeType -> String
funName p t =
  show t ++ "_" ++ show p

translateEnv :: ExpEnv -> TypeEnv -> VarEnv -> HaskellCode
translateEnv eenv tenv venv =
  let (m, ast1) = topExps eenv tenv venv
      s = execState (tMapWithKeyM addCode eenv) (initialState m ast1) in
    -- trace (showAST (ast s) ++ "\n\n")
--    trace (show (annFunMap s) ++ "\n\n")
     haskellCode s ++ "\n\n" ++ showGenFunMap (genFunsMap s)

-- TMP:
showAST :: AST -> String
showAST = Map.foldlWithKey (\acc e t -> acc ++ "(" ++ show (position e) ++ " ~ " ++ show e ++ ", " ++ show t ++ ") " ) "< "

addCode :: ProgVar -> Expression -> TranslateM ()
addCode f e = do
  m <- getAnnFunMap
  ast <- getAST
  (c,_) <- translate m ast (nodeType f m) e
  addHaskellCode f c
  where
    nodeType f m =
        case m Map.!? f of
              Just t -> lastType t
              Nothing-> PureType
              
isFunction :: ProgVar -> AnnFunMap -> Bool
isFunction = Map.member 

translateExpr :: NodeType -> NodeType -> HaskellCode -> TranslateM HaskellCode
translateExpr IOType PureType h = return $ "return " ++ h
translateExpr _ _ h = return h
              
translate :: AnnFunMap -> AST -> NodeType -> Expression -> TranslateM (HaskellCode, NodeType)
-- Basic values
-- I will not get from the map because a basic type is always pure
translate _ _ t (Unit _) = do 
  c <- translateExpr t PureType "()"
  return (c, t)
translate _ _ t (Integer _ i) = do
  c <- translateExpr t PureType (show i)
  return (c, t)
translate _ _ t (Character _ c) = do
  c <- translateExpr t PureType (show c)
  return (c, t)
translate _ _ t (Boolean _ b) = do
  c <- translateExpr t PureType (show b)
  return (c, t)

-- Variable
translate m ast nt e@(ProgVar _ x) = do
  if not (isFunction x m) then do
     -- let t1 = ast Map.! e
     -- c <- translateExpr nt t1 (show x)
     c <- translateExpr nt PureType (show x)
     return (c, nt)
  else do -- magic    
    let t = (m Map.! x)
    let mat = typeMatch (max t nt) (ast Map.! e)
--    traceM ("Function " ++ show x ++": t value " ++ show t ++ " ast value " ++ show (ast Map.! e) ++ " mat value " ++ show mat) 
    if t /= mat then do -- magic
      let types = zipNodeTypes t mat 
      let fname = funName x mat
      createNewFun fname x types 
      return (fname, IOType)
    else do
      let t1 = ast Map.! e
      return (show x, t1)
-- Abstraction intro and elim
translate m ast t (Lambda _ _ x _ e) = do
  (h1,nt) <- translate m ast t e
  return ("(\\" ++ show x ++ " -> " ++ h1 ++ ")", nt)
  
translate m ast nt e@(App _ e1 e2) = do
  (h1,t1) <- translate m ast PureType e1 -- TODO: nt?? PureType??
  (h2,t2) <- translate m ast PureType e2
  -- traceM ("\nTRANSLATE:" ++ show e++"\nnt: " ++ show nt ++ "\n" ++
  --        "t1 " ++ show t1 ++ "\n" ++
  --        "t2 " ++ show t2 ++ "\n" ++
  --        "max t1 t2 " ++ show (max t1 t2) ++ "\n" ++
  --        "max nt (max t1 t2) " ++ show (max nt (max t1 t2)) ++ "\n"
  --        )
  return ("(" ++ h1 ++ " " ++ h2 ++ ")", lastType $ max nt (max t1 t2)) -- TODO: max???


-- Pair intro and elim
translate m ast nt e@(Pair _ e1 e2) = do
  let t = ast Map.! e
  (h1,t1) <- translate m ast PureType e1
  (h2,t2) <- translate m ast PureType e2

  if | isIO t1 && isIO t2 -> do
        f1 <- nextFresh
        f2 <- nextFresh
        return (h1 ++ " >>= \\" ++ f1 ++ " -> " ++
                h2 ++ " >>= \\" ++ f2 ++
                " -> return (" ++ f1 ++ ", " ++ h2 ++ ")"
               , IOType)
     | isIO t1 -> do
        f <- nextFresh
        return (h1 ++ " >>= \\" ++ f ++ " -> return (" ++ f ++ ", " ++ h2 ++ ")"
               , IOType)
     | isIO t2 -> do
        f <- nextFresh
        return (h2 ++ " >>= \\" ++ f ++ " -> return (" ++ h1 ++ ", " ++ f ++ ")"
               , IOType)
     | isIO nt ->
         return ("return (" ++ h1 ++ ", " ++ h2 ++ ")", PureType)
     | otherwise ->
         return ("(" ++ h1 ++ ", " ++ h2 ++ ")", PureType)
         
translate m ast t e@(BinLet _ x y e1 e2) = do
  (h1, nt1) <- translate m ast t e1
  (h2, nt2) <- translate m ast t e2
  if isIO nt1 then
    return (h1 ++ " >>= \\(" ++ show x ++ ", " ++ show y ++ ") -> " ++ h2, IOType)
  else
    return ("(let (" ++ show x ++ ", " ++ show y ++ ") = " ++ h1 ++ " in " ++ h2 ++ ")", max nt1 nt2)
-- Datatype elim
translate m ast nt (Case _ e cm) =  do
  (h1,t1) <- translate m ast PureType e 
  (hcase, t2) <- translateCaseMap m ast nt cm
  return ("case " ++ h1 ++ " of {" ++ hcase ++ "}", max t1 t2) -- TODO: Can be monadic

-- Type application
translate m ast nt e@(TypeApp _ x _) = do -- EQUAL TO ProgVar ????
    let t = (m Map.! x)
    let mat = typeMatch t (ast Map.! e)
--        let mat = typeMatch (max t nt) (ast Map.! e)
    -- traceM ("\nTypeApp: " ++ show x ++ "\nt value " ++ show t ++
    --         "\nAST value: " ++ show (ast Map.! e) ++ "\n" ++
    --         "\nmatch value " ++ show mat ++ "\n")
    if t /= mat then do -- magic
      let types = zipNodeTypes t mat
      let fname = funName x mat
      createNewFun fname x types 
      return (fname, IOType)
    else do
      let t1 = ast Map.! e
      return (show x, t1)  

-- Boolean elim
translate m ast t (Conditional _ e1 e2 e3) = do
  (h1, t1) <- translate m ast PureType e1
  let maxTT1 = max t t1
  (h2, t2) <- translate m ast maxTT1 e2
  let maxT1T2 = max maxTT1 t2
  (h3, t3) <- translate m ast maxT1T2 e3
  let maxT = max maxT1T2 t3
  if isIO t1 then do
    fresh <- nextFresh
    return (h1 ++ " >>= \\" ++ fresh ++ " -> "
            ++ "if " ++ fresh ++ " then "
            ++ h2 ++ " else " ++ h3, IOType)
  else
    return ("if " ++ h1 ++ " then "
            ++ h2 ++ " else " ++ h3, maxT)

-- Let
translate m ast t e@(UnLet _ x e1 e2) = do
  (h1, nt1) <- translate m ast t e1
  (h2, nt2) <- translate m ast t e2
  if isIO nt1 then
    return (h1 ++ " >>= \\" ++ show x ++ " -> " ++ h2, IOType)
  else
    return ("(let " ++ show x ++ " = " ++ h1 ++ " in " ++ h2 ++ ")", max nt1 nt2)

-- Fork
translate m ast _ (Fork _ e) = do
  (h1, _) <- translate m ast IOType e
  return ("_fork (" ++ h1 ++ " >> return ())", IOType)

-- Session types
translate _ _ _ (New _ _) = return ("_new", IOType)
translate m ast t (Send _ e) = do
  (h1, t1) <- translate m ast PureType e
  if isIO t1 then do
    f <- nextFresh
    return (h1 ++ " >>= \\" ++ f ++ " -> (_send " ++ f ++ ")", IOType) 
  else  
    return ("(_send " ++ h1 ++ ")", IOType)

translate m ast t (Receive _ e) = do
  (h1, t1) <- translate m ast PureType e
  if isIO t1 then do
    f <- nextFresh
    return (h1 ++ " >>= \\" ++ f ++ " -> (_receive " ++ f ++ ")", IOType) 
  else  
    return ("(_receive " ++ h1 ++ ")", IOType)

translate m ast nt e@(Select _ e1 x) = do
  (h, _) <- translate m ast PureType e1 -- PureType or nt??
  return ("(_send " ++ h ++ " \"" ++ show x ++ "\")", IOType)
  
translate m ast nt (Match _ e mm) = do
  (h1, t1) <- translate m ast PureType e 
  v <- nextFresh
  fresh <- nextFresh
--  traceM ("NT:" ++ show nt)
  (h2, t2) <- translateMatchMap fresh m ast nt mm  
  return ("_receive " ++ h1 ++ " >>= \\(" ++ v ++  ", " ++ fresh ++
          ") -> (case " ++ v ++ " of {" ++ h2 ++ "})", max t1 t2)   
  
typeMatch :: NodeType -> NodeType -> NodeType
typeMatch (ArrowType s1 s2) (ArrowType s3 s4) = ArrowType (max s1 s3) (typeMatch s2 s4)
typeMatch t1 t2 = max t1 t2

translateCaseMap :: AnnFunMap -> AST -> NodeType -> FieldMap -> TranslateM (String, NodeType)
translateCaseMap m ast nt =
  Map.foldlWithKey translateCaseMap' (return ("", PureType))
  where
    translateCaseMap' :: TranslateM (String, NodeType) -> ProgVar ->
                         ([ProgVar], Expression) -> TranslateM (String, NodeType)
    translateCaseMap' acc cons (ps, e) = do
      (h, n1) <- translate m ast nt e
      (acc', n2) <- acc
      return (acc' ++ "\n    " ++ show cons ++ showCaseParams ps ++ " -> " ++ h ++ ";",
              max n1 n2)

    showCaseParams :: [ProgVar] -> String
    showCaseParams [] = ""
    showCaseParams args = foldl (\acc a -> acc ++ " " ++ show a) "" args


translateMatchMap :: String -> AnnFunMap -> AST -> NodeType -> FieldMap -> TranslateM (String, NodeType)
translateMatchMap fresh m ast nt =
  Map.foldlWithKey (translateMatchMap' fresh) (return ("", PureType))
  where
    translateMatchMap' :: String -> TranslateM (String, NodeType) -> ProgVar ->
                         ([ProgVar], Expression) -> TranslateM (String, NodeType)
    translateMatchMap' fresh acc v (p:_, e) = do
      (h, n1) <- translate m ast nt e
      (acc', n2) <- acc
      return (acc' ++ "\n    \"" ++ show v ++ "\" " ++
        " -> let " ++ show p ++ " = " ++ fresh ++ " in " ++ h ++ ";", max n1 n2)
             

-- TODO: Think about alternatives to this problem
updateEEnv :: ExpEnv -> ExpEnv
updateEEnv = Map.foldrWithKey (\f e acc -> Map.insert f (updateEEnv' e) acc) Map.empty

updateEEnv' :: Expression -> Expression
updateEEnv' e@(ProgVar p x) =
  if show x == "main" then (ProgVar p (mkVar p "_main"))
  else e

updateEEnv' (Lambda p m x t e) = Lambda p m x t (updateEEnv' e)
updateEEnv' (App p e1 e2) = (App p (updateEEnv' e1) (updateEEnv' e2))
updateEEnv' (Pair p e1 e2) = (Pair p (updateEEnv' e1) (updateEEnv' e2))
updateEEnv' (BinLet p x y e1 e2) = (BinLet p x y (updateEEnv' e1) (updateEEnv' e2))
-- updateEEnv' (Case Pos Expression FieldMap

updateEEnv' (Conditional p e1 e2 e3) = (Conditional p (updateEEnv' e1) (updateEEnv' e2) (updateEEnv' e3))
updateEEnv' (UnLet p x e1 e2) = (UnLet p x (updateEEnv' e1) (updateEEnv' e2))
updateEEnv' (Fork p e) = (Fork p (updateEEnv' e))
updateEEnv' (Send p e) = (Send p (updateEEnv' e))
updateEEnv' (Receive p e) = (Receive p (updateEEnv' e))
updateEEnv' (Select p e x) = (Select p (updateEEnv' e) x)
-- updateEEnv' (Match Pos Expression FieldMap
updateEEnv' e = e
