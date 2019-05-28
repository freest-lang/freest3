{-# LANGUAGE MultiWayIf #-}
module CodeGen.Annotation where

import           Syntax.Expressions
import           Syntax.Base -- Test
import           Syntax.Types -- Test
import           Syntax.Show -- Test ?
import           Syntax.Kinds -- Test
import           Syntax.Schemes
import           Syntax.ProgramVariables
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import Debug.Trace -- DEbug
import  Utils.FreestState (tMapWithKeyM)
import qualified Data.Traversable as Traversable

import Utils.PreludeLoader
type AST = Map.Map Expression NodeType
type AnnFunMap = Map.Map ProgVar NodeType

data NodeType =
    IOType
  | PureType
  | ArrowType NodeType NodeType
  deriving Eq

--  -- Show debug
-- instance Show NodeType where
--   show IOType = "IOType"
--   show PureType = "PureType"
-- --  show (ArrowType t1 t2) = show t1 ++ " -> " ++ show t2
--   show (ArrowType t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

-- Show fun create
instance Show NodeType where
  show IOType = "io"
  show PureType = "p"
  show (ArrowType t1 t2) = show t1 ++ "_" ++ show t2
 
instance Ord NodeType where
  IOType   <= (ArrowType _ _) = True  
  PureType <= IOType          = True  
  _        <= _               = False

isIO :: NodeType -> Bool
isIO (ArrowType _ s2) = isIO s2
isIO IOType           = True
isIO _                = False

-- Annotation of the variable environment

-- TODO: CHANGE NAME

initialEnv :: VarEnv -> AnnFunMap
initialEnv = Map.foldrWithKey (\k t m -> Map.insert k (funToArrow t) m) Map.empty

funToArrow :: TypeScheme -> NodeType
funToArrow (TypeScheme _ _ (Fun _ _ t1 t2)) =
  ArrowType (funToArrow (fromType t1)) (funToArrow (fromType t2))
funToArrow _ = PureType

top :: ExpEnv -> TypeEnv -> VarEnv -> AnnFunMap
top eenv tenv venv = findFixedPoint eenv (initialEnv venv) -- (annotateVenv tenv venv)

findFixedPoint :: ExpEnv -> AnnFunMap -> AnnFunMap
findFixedPoint eenv avenv
  | avenv == avenv' = avenv
  | otherwise = findFixedPoint eenv avenv'
  where avenv' = annotateFunction avenv eenv



annotateFunction :: AnnFunMap -> ExpEnv -> AnnFunMap
annotateFunction = Map.foldrWithKey insert -- (\f t m -> insert f t m)
  where insert f t m = Map.adjust (updateLastType (annFun m t)) f m
--          Map.insert f (annFun m e) m
          -- let (t, m1) = annFun m e in
          -- Map.insert f (normIO t) m1 -- f m

updateLastType :: NodeType -> NodeType -> NodeType
updateLastType exp a@(ArrowType t1 t2)  = 
  ArrowType t1 (updateLastType exp t2)
updateLastType exp t = max exp t



annFun :: AnnFunMap -> Expression -> NodeType
annFun _ (Unit _) = PureType
annFun _ (Integer _ _) = PureType
annFun _ (Character _ _) = PureType
annFun _ (Boolean _ _) = PureType
annFun m (ProgVar _ x) = annVar m x
annFun m l@(Lambda _ _ _ _ e) =
  annFun m e
annFun m (App _ e1 e2) = max (annFun m e1) (annFun m e2)
annFun m (TypeApp _ x _) = annVar m x
annFun m (Pair _ e1 e2) = max (annFun m e1) (annFun m e2)
annFun m (BinLet _ _ _ e1 e2) = max (annFun m e1) (annFun m e2)
annFun m (UnLet _ _ e1 e2) = max (annFun m e1) (annFun m e2)
annFun m (Conditional _ e1 e2 e3) =
  max (annFun m e1) (max (annFun m e2) (annFun m e3))
annFun m (Case _ e cm) =
  Map.foldr (\ce acc -> max acc (annFun m (snd ce))) (annFun m e) cm
-- Session types & Fork
annFun m _ = IOType 

annVar :: AnnFunMap -> ProgVar -> NodeType
annVar m x =
  case m Map.!? x of
    Just t  -> lastType t
    Nothing -> PureType


lastType :: NodeType -> NodeType
lastType (ArrowType _ s) = lastType s
lastType s = s

topExps :: ExpEnv -> TypeEnv -> VarEnv -> (AnnFunMap, AST)
topExps eenv tenv venv =
    let funMap = fm in (funMap, findExpFixedPoint funMap eenv Map.empty)
  where fm = top eenv tenv venv
  
-- type ExpEnv = Map.Map ProgVar Expression
findExpFixedPoint :: AnnFunMap -> ExpEnv -> AST -> AST
findExpFixedPoint m eenv ast
  | ast == ast' = ast
  | otherwise = findExpFixedPoint m eenv ast'
  where ast' = annotateAST m ast eenv

-- annotateAST m = Map.foldrWithKey (\f e ast -> fst $ annExp m ast (nodeType f)  e)
annotateAST :: AnnFunMap -> AST -> ExpEnv -> AST
annotateAST m = Map.foldrWithKey (\f e ast -> fst $ annExp m ast (nodeType f)  e)
   where nodeType f = -- lastType (m Map.! f)
            case m Map.!? f of
              Just t -> lastType t
              Nothing -> PureType

annPVar :: AnnFunMap -> ProgVar -> NodeType
annPVar m x =
  case m Map.!? x of
    Just t  -> t
    Nothing -> PureType

updateParam :: NodeType -> NodeType -> NodeType
updateParam IOType (ArrowType _ t2) = ArrowType IOType (updateLastType IOType t2)
updateParam _ t                     = t

annExp :: AnnFunMap -> AST -> NodeType -> Expression -> (AST, NodeType)
-- Basic values
annExp _ ast t e@(Unit _)                  = (Map.insert e PureType ast, PureType)
annExp _ ast t e@(Integer _ _)             = (Map.insert e PureType ast, PureType)
annExp _ ast t e@(Character _ _)           = (Map.insert e PureType ast, PureType)
annExp _ ast t e@(Boolean _ _)             = (Map.insert e PureType ast, PureType)
-- Variable
annExp fm ast t e@(ProgVar _ x)            =
  let t1 = annPVar fm x
      maxt = lastType t1 in --updateLastType t t1 in
      (Map.insert e t1 ast, t1)
-- Abstraction intro and elim  
annExp fm ast t e@(Lambda _ _ x _ e1) =
  let (ast1, t1) = annExp fm ast t e1
      maxt       = ArrowType PureType (max t t1) in
    (Map.insert e maxt ast1, maxt) -- TODO: max??

annExp fm ast t e@(App _ e1 e2) =
  let (ast1, t1) = annExp fm ast t e1
      (ast2, t2) = annExp fm ast1 PureType e2
      lt2 = (lastType t2)
      t3 = (updateLastType (max t lt2) (updateParam lt2 t1))
    --  t3 = (updateParam lt2 t1)
--      t4 = (updateLastType (max t t2) (updateParam t2 t1))
      ast3 = Map.insert e1 t3 ast2 in -- This is tricky (2nd insert) 
    (Map.insert e (lastType t3) ast3, lastType t3)
      
-- Pair intro and elim    
annExp fm ast t e@(Pair _ e1 e2) =
  let (ast1, t1) = annExp fm ast t e1
      (ast2, t2) = annExp fm ast1 t e2
      maxt1      = max t (max t1 t2) in
      (Map.insert e maxt1 ast2, maxt1)
      
annExp fm ast t e@(BinLet _ _ _ e1 e2) =
  let (ast1, t1) = annExp fm ast t e1
      maxt1      = max t t1
      (ast2, t2) = annExp fm ast1 maxt1 e2
      maxt2       = max maxt1 t2 in
    (Map.insert e maxt2 ast2, maxt2)    
-- Datatype elim  
annExp fm ast t e@(Case _ e1 cm) =
  let (ast1, t1) = annExp fm ast t e1
      (ast2, t2) =
        Map.foldr (\ce (acc, t1) -> let (x,y) = annExp fm acc t (snd ce) in
                      (x, max y t1)) (ast1, PureType) cm
      maxT = max t1 t2
  in (Map.insert e maxT ast2, maxT)               
          
-- Type application
annExp fm ast t e@(TypeApp _ x _) =
  let t1 = annPVar fm x
      maxt = lastType t1 in --updateLastType t t1 in
      (Map.insert e t1 ast, t1)
  
-- Boolean elim
annExp fm ast t e@(Conditional _ e1 e2 e3) =
  let (ast1, t1) = annExp fm ast t e1
      (ast2, t2) = annExp fm ast1 t e2
      (ast3, t3) = annExp fm ast2 t e3
      maxT       = max t1 (max t2 t3) in
      (Map.insert e maxT ast3, maxT)

-- Let  
annExp fm ast t e@(UnLet (Pos p _) _ e1 e2) =
  let (ast1, t1) = annExp fm ast t e1
      maxt1      = max t t1
      (ast2, t2) = annExp fm ast1 maxt1 e2
      maxt2       = max maxt1 t2 in
    (Map.insert e maxt2 ast2, maxt2)  
  
-- Fork
annExp fm ast t e@(Fork _ e1) = 
  let ast1 = annExp fm ast IOType e1 in
    (Map.insert e IOType (fst ast1), IOType)  

-- Session Types
annExp _ ast _ e@(New _ _) = (Map.insert e IOType ast, IOType)
annExp fm ast _ e@(Receive _ e1) =
  let (ast1,_) = annExp fm ast PureType e1 in
    (Map.insert e IOType ast1, IOType)
annExp fm ast _ e@(Send _ e1) =
  let (ast1,_) = annExp fm ast PureType e1 in
    (Map.insert e IOType ast1, PureType)    
annExp fm ast t e@(Match _ e1 cm) =
  let (ast1, t1) = annExp fm ast PureType e1
      (ast2, t2) =
        Map.foldr (\ce (acc, t1) -> let (x,y) = annExp fm acc t (snd ce) in
                      (x, max y t1)) (ast1, PureType) cm
      maxT = max t1 t2
  in (Map.insert e maxT ast2, maxT)
  
annExp fm ast t e@(Select _ x e1) =
  let (ast1,_) = annExp fm ast PureType e1 in
    (Map.insert e IOType ast1, IOType)
 


type HaskellCode = String
type GenFunsMap = Map.Map String HaskellCode

data TranslateState = TranslateState {
  nextIndex  :: Int
, genFunsMap :: GenFunsMap
, haskellCode :: HaskellCode
, annFunMap :: AnnFunMap
, ast :: AST
}

type TranslateM = State TranslateState

initialState :: AnnFunMap -> AST -> TranslateState
initialState m ast = TranslateState {
  nextIndex  = 0
, genFunsMap = Map.empty
, haskellCode = ""
, annFunMap = m
, ast = ast
}
  
-- Gets the next fresh var based on the state
nextFresh :: TranslateM String
nextFresh = do
  s <- get
  let next = nextIndex s
  modify (\s -> s{nextIndex = next + 1})
  return $ "_x" ++ show next

-- Adds an element to the map
addFun :: String -> HaskellCode -> TranslateM ()
addFun k v = do
  modify (\s -> s{genFunsMap = Map.insert k v (genFunsMap s)})

funMember :: String -> TranslateM Bool
funMember f = do
  m <- getGenFunsMap
  return $ Map.member f m

getGenFunsMap :: TranslateM GenFunsMap
getGenFunsMap = do 
  s <- get
  return $ genFunsMap s

getAnnFunMap :: TranslateM AnnFunMap
getAnnFunMap = do 
  s <- get
  return $ annFunMap s

getAST :: TranslateM AST
getAST = do 
  s <- get
  return $ ast s  

getHaskellCode :: TranslateM HaskellCode
getHaskellCode = do
  s <- get
  return $ haskellCode s
  
addHaskellCode :: ProgVar -> HaskellCode -> TranslateM ()
addHaskellCode fname c =  
  modify (\s -> s{haskellCode = (haskellCode s) ++ "\n\n" ++
                   show fname ++ " = " ++ c})
  

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
  let (m, ast) = topExps eenv tenv venv
      s = execState (tMapWithKeyM addCode eenv) (initialState m ast) in
     haskellCode s ++ "\n\n" ++ showGenFunMap (genFunsMap s)

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
translate m ast t e@(ProgVar _ x) = do
  if not (isFunction x m) then do
     let t1 = ast Map.! e
     c <- translateExpr t PureType (show x)
     return (c, t)
  else do -- magic
    let t = (m Map.! x)
    let mat = typeMatch t (ast Map.! e)
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
  return ("(" ++ h1 ++ " " ++ h2 ++ ")", max nt (max t1 t2)) -- TODO: max???


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
translate m ast t e@(TypeApp _ x _) = do -- EQUAL TO ProgVar ????
    let t = (m Map.! x)
    let mat = typeMatch t (ast Map.! e)
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

translate m ast nt e@(Select _ x e1) = do
  (h, _) <- translate m ast PureType e1 -- PureType or nt??
  return ("(_send " ++ h ++ " \"" ++ show x ++ "\")", IOType)
  
translate m ast nt (Match _ e mm) = do
  (h1, t1) <- translate m ast PureType e 
  v <- nextFresh
  fresh <- nextFresh
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
updateEEnv' (Select p x e) = (Select p x (updateEEnv' e))
-- updateEEnv' (Match Pos Expression FieldMap
updateEEnv' e = e
