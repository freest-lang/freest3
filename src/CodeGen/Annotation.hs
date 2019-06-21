module CodeGen.Annotation where

import           CodeGen.CodeGenState
import           Control.Monad.State
import           Syntax.Expressions
import           Syntax.Base -- Test
import           Syntax.Types -- Test
import           Syntax.Show -- Test ?
import           Syntax.Kinds -- Test
import           Syntax.Schemes
import           Syntax.ProgramVariables
import           Data.List
import qualified Data.Map.Strict as Map
import Debug.Trace -- DEbug
import qualified Data.Traversable as Traversable

import Utils.PreludeLoader

class ShowD t where
  showD :: t -> String
  
instance ShowD NodeType where
  showD IOType = "IOType"
  showD PureType = "PureType"
--  showD (ArrowType t1 t2) = showD t1 ++ " -> " ++ showD t2
  showD (ArrowType t1 t2) = "(" ++ showD t1 ++ " -> " ++ showD t2 ++ ")"

-- Annotation of the variable environment

-- TODO: CHANGE NAME

initialEnv :: VarEnv -> AnnFunMap
initialEnv = Map.foldrWithKey (\k t m -> Map.insert k (funToArrow t) m) Map.empty

funToArrow :: TypeScheme -> NodeType
funToArrow (TypeScheme _ _ (Fun _ _ t1 t2)) =
 ArrowType (funToArrow (fromType t1)) (funToArrow (fromType t2))
--  ArrowType PureType (funToArrow (fromType t2))
funToArrow _ = PureType

top :: ExpEnv -> TypeEnv -> VarEnv -> AnnFunMap
top eenv tenv venv =
  findFixedPoint eenv venv (initialEnv venv) -- (annotateVenv tenv venv)

findFixedPoint :: ExpEnv -> VarEnv -> AnnFunMap -> AnnFunMap
findFixedPoint eenv venv avenv
  | avenv == avenv' = avenv
  | otherwise = findFixedPoint eenv venv avenv'
  where avenv' = annotateFunction venv avenv eenv



annotateFunction :: VarEnv -> AnnFunMap -> ExpEnv -> AnnFunMap
annotateFunction venv = Map.foldrWithKey insert -- (\f t m -> insert f t m)
  where insert f e m = Map.adjust (updateLT (venv Map.! f) (annFun m e)) f m
--  where insert f t m = Map.adjust (updateLastType (annFun m t)) f m

updateLT :: TypeScheme -> NodeType -> NodeType -> NodeType
updateLT (TypeScheme _ _ f@(Fun _ _ _ _)) exp (ArrowType a@(ArrowType _ _) t2)  =
  ArrowType (updateLastType (checkType f) a) (updateLastType exp t2)            
updateLT _ exp (ArrowType t1 t2)  = 
  ArrowType t1 (updateLastType exp t2)
updateLT _ exp t = max exp t

updateLastType :: NodeType -> NodeType -> NodeType
updateLastType exp (ArrowType t1 t2)  = 
  ArrowType t1 (updateLastType exp t2)
updateLastType exp t = max exp t


checkType :: Type -> NodeType
checkType (Skip _) = IOType
checkType (Semi _ _ _) = IOType
checkType (Message _ _ _) = IOType
checkType (Choice _ _ _) = IOType
checkType (Fun _ _ t1 t2) = max (checkType t1) (checkType t2) -- Think
checkType _ = PureType
            

annFun :: AnnFunMap -> Expression -> NodeType
annFun _ (Unit _) = PureType
annFun _ (Integer _ _) = PureType
annFun _ (Character _ _) = PureType
annFun _ (Boolean _ _) = PureType
annFun m (ProgVar _ x) = annVar m x
annFun m (Lambda _ _ _ _ e) = annFun m e
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
    let funMap = fm in (funMap, annotateAST funMap Map.empty eenv)-- findExpFixedPoint funMap eenv Map.empty)
  where fm = top eenv tenv venv
  
-- -- type ExpEnv = Map.Map ProgVar Expression
-- findExpFixedPoint :: AnnFunMap -> ExpEnv -> AST -> AST
-- findExpFixedPoint m eenv ast
--   | ast == ast' = ast
--   | otherwise = findExpFixedPoint m eenv ast'
--   where ast' = annotateAST m ast eenv

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
    Just t  -> {-trace ("function "++ show x ++ " at " ++ show (position x) ++ " -> " ++ show t  ) -} t
    Nothing -> PureType

updateParam :: NodeType -> NodeType -> NodeType
updateParam IOType (ArrowType _ t2) = ArrowType IOType (updateLastType IOType t2)
updateParam _ t                     = t


-- TODO: Use state
annExp :: AnnFunMap -> AST -> NodeType -> Expression -> (AST, NodeType)
-- Basic values
annExp _ ast t e@(Unit _)                  = (Map.insert e PureType ast, PureType)
annExp _ ast t e@(Integer _ _)             = (Map.insert e PureType ast, PureType)
annExp _ ast t e@(Character _ _)           = (Map.insert e PureType ast, PureType)
annExp _ ast t e@(Boolean _ _)             = (Map.insert e PureType ast, PureType)
-- Variable
annExp fm ast t e@(ProgVar _ x)            =
  let t1 = annPVar fm x in
--      maxt = lastType t1 in --updateLastType t t1 in
      (Map.insert e t1 ast, t1)
-- Abstraction intro and elim  
annExp fm ast t e@(Lambda _ _ x _ e1) =
  let (ast1, t1) = annExp fm ast t e1
      maxt       = ArrowType PureType (max t t1) in
    (Map.insert e maxt ast1, maxt) -- TODO: max??

annExp fm ast t e@(App _ e1 e2) =
     -- trace ("Expression " ++ show e ++ " current state " ++ show t ++ " -> updating param " ++ show t1 ++ " with type (t2) " ++ show lt2 ++ " result " ++ show t3)
  let (ast1, t1) = annExp fm ast t e1
      (ast2, t2) = annExp fm ast1 PureType e2
      t3         = applyNodeType (max t2 t1) (min t2 t1) -- max and min ... should be like this?
      ast3       = updateProgVar ast2 e1 (updateLastType (max t (lastType t3)) t3) in
      -- trace ("Expression (e1): " ++ show e1 ++ " has type " ++ showD t1 ++ "\n" ++
      --       "Expression (e2): " ++ show e2 ++ " has type " ++ showD t2 ++ "\n" ++
      --       "max (max t2 t1): " ++ showD (max t2 t1) ++ "\n" ++
      --       "min (min t2 t1): " ++ showD (min t2 t1) ++ "\n" ++
      --       "the t value is " ++ showD t ++ "\n" ++
      --       "the final type is " ++ showD t3 ++ "\n" 
      --       )
      (Map.insert e (lastType t3) ast3, lType t3)
      
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
  let t1 = annPVar fm x in
--      maxt = lastType t1 in --updateLastType t t1 in
--    trace ("TYPEAPP " ++ show x ++ " has type: " ++ show t1)
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
  
annExp fm ast t e@(Select _ e1 x) =
  let (ast1,_) = annExp fm ast PureType e1 in
    (Map.insert e IOType ast1, IOType)
 


-- TODO: Check if need to remove the other last type, it makes sense?
-- if so, rename this into lastType
lType :: NodeType -> NodeType
lType (ArrowType _ t) = t
lType t               = t

-- TODO: change name
applyNodeType :: NodeType -> NodeType -> NodeType
applyNodeType t1 t2 = normaliseType (applyNodeType' t1 t2)

applyNodeType' :: NodeType -> NodeType -> NodeType
applyNodeType' a@(ArrowType _ _) (ArrowType b@(ArrowType _ _) s4) = ArrowType (applyNodeType' a b) s4
--applyNodeType' (ArrowType  a@(ArrowType _ _) s1) b@(ArrowType _ _) = ArrowType (applyNodeType' b a) s1
--applyNodeType' (ArrowType s1 s2) (ArrowType s3 s4) = ArrowType (applyNodeType' s1 s3) (applyNodeType' s2 s4)2
applyNodeType' (ArrowType _ s2)  t                 = ArrowType t s2
applyNodeType' t                 u                 = max t u


normaliseType :: NodeType -> NodeType
normaliseType t
  | anyIO t    = updateLastType IOType t 
  | otherwise = t


-- TODO: Monad version, uncomment when annExps turns monad
-- updateProgVar :: Expression -> NodeType -> TranslateM ()
-- updateProgVar (App _ e _) t = updateProgVar e t
-- updateProgVar e@(ProgVar _ _) t1 = do
--    t2 <- getFromAST e
--    insertAST e (zipFromEnd t2 t1)

updateProgVar :: AST -> Expression -> NodeType -> AST
updateProgVar m (App _ e _) t = updateProgVar m e t
updateProgVar m e@(ProgVar _ _) t1 =
   let t2 = m Map.! e in
   Map.insert e (zipFromEnd t2 t1) m 
updateProgVar m e@(TypeApp _ _ _) t1 =
   let t2 = m Map.! e in
   -- trace ("\nUPDATE TYPE APP : " ++ show e ++
   --        "\n to update Type " ++ show t1 ++
   --        "\n astType " ++ show t2 ++
   --        "\n zipedType " ++ show (zipFromEnd t2 t1) ++ "\n")
   Map.insert e (zipFromEnd t2 t1) m
updateProgVar m _ _ = m
  
anyIO :: NodeType -> Bool
anyIO (ArrowType s1 s2) = anyIO s1 || anyIO s2
anyIO IOType           = True
anyIO _                = False

zipFromEnd :: NodeType -> NodeType -> NodeType
zipFromEnd t1 t2 = 
  foldr (\t acc -> ArrowType t acc) t2
    (nodeTypeTake (nodeTypeLength t1 - nodeTypeLength t2) t1)
  


nodeTypeLength :: NodeType -> Int
-- nodeTypeLength (ArrowType t1 t2) = nodeTypeLength t1 + nodeTypeLength t2
nodeTypeLength (ArrowType t1 t2) = 1 + nodeTypeLength t2
nodeTypeLength t                 = 1

nodeTypeTake :: Int -> NodeType -> [NodeType]
nodeTypeTake 0 _ = []
nodeTypeTake n (ArrowType t1 t2) = t1 : nodeTypeTake (n-1) t2
nodeTypeTake _ t = [t]
  

-- -- Creates a new function based on the types
-- -- createNewFun :: Function name -> function to call ->
-- --                 [(Expected type, actual type)] -> haskell code
-- createNewFun :: String -> ProgVar -> [(NodeType, NodeType)] -> TranslateM ()
-- createNewFun s x xs = do
--   m <- getGenFunsMap
--   if s `Map.member` m
--     then return ()
--     else do
--       params <- sequence $ take (length xs-1) (repeat (nextFresh))
--       c <- genFunCode x xs params []
--       let ps = foldl (\acc a -> acc ++ "\\" ++ a ++ " -> ") "" params
--       addFun s (ps ++ c)

-- genFunCode :: ProgVar -> [(NodeType, NodeType)] -> [String] -> [String] -> TranslateM HaskellCode
-- genFunCode pv ((x,y):[]) _ ys
--   | x /= y = return $ "return (" ++ foldl (\acc a -> acc ++ " " ++ a) (show pv) ys ++ ")"
--   | otherwise = return $ foldl (\acc a -> acc ++ " " ++ a) (show pv) ys
-- genFunCode pv ((x1,x2):xs) (y:ys) acc
--   | x1 /= x2 = do
--       f <- nextFresh   
--       let c1 = y ++ " >>= \\" ++ f ++ " -> "
--       c2 <- genFunCode pv xs ys (acc ++ [f])
--       return $ c1 ++ c2
--   | otherwise = genFunCode pv xs ys (acc ++ [y])
-- genFunCode pv xs ys acc = error $ show acc

-- zipNodeTypes :: NodeType -> NodeType -> [(NodeType, NodeType)]
-- zipNodeTypes (ArrowType s1 s2) (ArrowType s3 s4) = (s1, s3) : zipNodeTypes s2 s4
-- zipNodeTypes t1 t2 = [(t1, t2)]

-- showGenFunMap :: GenFunsMap -> String
-- showGenFunMap = Map.foldlWithKey (\acc h b -> acc ++ "\n\n" ++ h ++ " = " ++ b) ""

-- funName :: ProgVar -> NodeType -> String
-- funName p t =
--   show t ++ "_" ++ show p

-- translateEnv :: ExpEnv -> TypeEnv -> VarEnv -> HaskellCode
-- translateEnv eenv tenv venv =
--   let (m, ast1) = topExps eenv tenv venv
--       s = execState (tMapWithKeyM addCode eenv) (initialState m ast1) in
--     -- trace (showAST (ast s) ++ "\n\n")
-- --    trace (show (annFunMap s) ++ "\n\n")
--      haskellCode s ++ "\n\n" ++ showGenFunMap (genFunsMap s)

-- -- TMP:
-- showAST :: AST -> String
-- showAST = Map.foldlWithKey (\acc e t -> acc ++ "(" ++ show (position e) ++ " ~ " ++ show e ++ ", " ++ show t ++ ") " ) "< "

-- addCode :: ProgVar -> Expression -> TranslateM ()
-- addCode f e = do
--   m <- getAnnFunMap
--   ast <- getAST
--   (c,_) <- translate m ast (nodeType f m) e
--   addHaskellCode f c
--   where
--     nodeType f m =
--         case m Map.!? f of
--               Just t -> lastType t
--               Nothing-> PureType
              
-- isFunction :: ProgVar -> AnnFunMap -> Bool
-- isFunction = Map.member 

-- translateExpr :: NodeType -> NodeType -> HaskellCode -> TranslateM HaskellCode
-- translateExpr IOType PureType h = return $ "return " ++ h
-- translateExpr _ _ h = return h
              
-- translate :: AnnFunMap -> AST -> NodeType -> Expression -> TranslateM (HaskellCode, NodeType)
-- -- Basic values
-- -- I will not get from the map because a basic type is always pure
-- translate _ _ t (Unit _) = do 
--   c <- translateExpr t PureType "()"
--   return (c, t)
-- translate _ _ t (Integer _ i) = do
--   c <- translateExpr t PureType (show i)
--   return (c, t)
-- translate _ _ t (Character _ c) = do
--   c <- translateExpr t PureType (show c)
--   return (c, t)
-- translate _ _ t (Boolean _ b) = do
--   c <- translateExpr t PureType (show b)
--   return (c, t)

-- -- Variable
-- translate m ast nt e@(ProgVar _ x) = do
--   if not (isFunction x m) then do
--      -- let t1 = ast Map.! e
--      -- c <- translateExpr nt t1 (show x)
--      c <- translateExpr nt PureType (show x)
--      return (c, nt)
--   else do -- magic    
--     let t = (m Map.! x)
--     let mat = typeMatch (max t nt) (ast Map.! e)
-- --    traceM ("Function " ++ show x ++": t value " ++ show t ++ " ast value " ++ show (ast Map.! e) ++ " mat value " ++ show mat) 
--     if t /= mat then do -- magic
--       let types = zipNodeTypes t mat 
--       let fname = funName x mat
--       createNewFun fname x types 
--       return (fname, IOType)
--     else do
--       let t1 = ast Map.! e
--       return (show x, t1)
-- -- Abstraction intro and elim
-- translate m ast t (Lambda _ _ x _ e) = do
--   (h1,nt) <- translate m ast t e
--   return ("(\\" ++ show x ++ " -> " ++ h1 ++ ")", nt)
  
-- translate m ast nt e@(App _ e1 e2) = do
--   (h1,t1) <- translate m ast PureType e1 -- TODO: nt?? PureType??
--   (h2,t2) <- translate m ast PureType e2
--   -- traceM ("\nTRANSLATE:" ++ show e++"\nnt: " ++ show nt ++ "\n" ++
--   --        "t1 " ++ show t1 ++ "\n" ++
--   --        "t2 " ++ show t2 ++ "\n" ++
--   --        "max t1 t2 " ++ show (max t1 t2) ++ "\n" ++
--   --        "max nt (max t1 t2) " ++ show (max nt (max t1 t2)) ++ "\n"
--   --        )
--   return ("(" ++ h1 ++ " " ++ h2 ++ ")", lastType $ max nt (max t1 t2)) -- TODO: max???


-- -- Pair intro and elim
-- translate m ast nt e@(Pair _ e1 e2) = do
--   let t = ast Map.! e
--   (h1,t1) <- translate m ast PureType e1
--   (h2,t2) <- translate m ast PureType e2

--   if | isIO t1 && isIO t2 -> do
--         f1 <- nextFresh
--         f2 <- nextFresh
--         return (h1 ++ " >>= \\" ++ f1 ++ " -> " ++
--                 h2 ++ " >>= \\" ++ f2 ++
--                 " -> return (" ++ f1 ++ ", " ++ h2 ++ ")"
--                , IOType)
--      | isIO t1 -> do
--         f <- nextFresh
--         return (h1 ++ " >>= \\" ++ f ++ " -> return (" ++ f ++ ", " ++ h2 ++ ")"
--                , IOType)
--      | isIO t2 -> do
--         f <- nextFresh
--         return (h2 ++ " >>= \\" ++ f ++ " -> return (" ++ h1 ++ ", " ++ f ++ ")"
--                , IOType)
--      | isIO nt ->
--          return ("return (" ++ h1 ++ ", " ++ h2 ++ ")", PureType)
--      | otherwise ->
--          return ("(" ++ h1 ++ ", " ++ h2 ++ ")", PureType)
         
-- translate m ast t e@(BinLet _ x y e1 e2) = do
--   (h1, nt1) <- translate m ast t e1
--   (h2, nt2) <- translate m ast t e2
--   if isIO nt1 then
--     return (h1 ++ " >>= \\(" ++ show x ++ ", " ++ show y ++ ") -> " ++ h2, IOType)
--   else
--     return ("(let (" ++ show x ++ ", " ++ show y ++ ") = " ++ h1 ++ " in " ++ h2 ++ ")", max nt1 nt2)
-- -- Datatype elim
-- translate m ast nt (Case _ e cm) =  do
--   (h1,t1) <- translate m ast PureType e 
--   (hcase, t2) <- translateCaseMap m ast nt cm
--   return ("case " ++ h1 ++ " of {" ++ hcase ++ "}", max t1 t2) -- TODO: Can be monadic

-- -- Type application
-- translate m ast nt e@(TypeApp _ x _) = do -- EQUAL TO ProgVar ????
--     let t = (m Map.! x)
--     let mat = typeMatch t (ast Map.! e)
-- --        let mat = typeMatch (max t nt) (ast Map.! e)
--     -- traceM ("\nTypeApp: " ++ show x ++ "\nt value " ++ show t ++
--     --         "\nAST value: " ++ show (ast Map.! e) ++ "\n" ++
--     --         "\nmatch value " ++ show mat ++ "\n")
--     if t /= mat then do -- magic
--       let types = zipNodeTypes t mat
--       let fname = funName x mat
--       createNewFun fname x types 
--       return (fname, IOType)
--     else do
--       let t1 = ast Map.! e
--       return (show x, t1)  

-- -- Boolean elim
-- translate m ast t (Conditional _ e1 e2 e3) = do
--   (h1, t1) <- translate m ast PureType e1
--   let maxTT1 = max t t1
--   (h2, t2) <- translate m ast maxTT1 e2
--   let maxT1T2 = max maxTT1 t2
--   (h3, t3) <- translate m ast maxT1T2 e3
--   let maxT = max maxT1T2 t3
--   if isIO t1 then do
--     fresh <- nextFresh
--     return (h1 ++ " >>= \\" ++ fresh ++ " -> "
--             ++ "if " ++ fresh ++ " then "
--             ++ h2 ++ " else " ++ h3, IOType)
--   else
--     return ("if " ++ h1 ++ " then "
--             ++ h2 ++ " else " ++ h3, maxT)

-- -- Let
-- translate m ast t e@(UnLet _ x e1 e2) = do
--   (h1, nt1) <- translate m ast t e1
--   (h2, nt2) <- translate m ast t e2
--   if isIO nt1 then
--     return (h1 ++ " >>= \\" ++ show x ++ " -> " ++ h2, IOType)
--   else
--     return ("(let " ++ show x ++ " = " ++ h1 ++ " in " ++ h2 ++ ")", max nt1 nt2)

-- -- Fork
-- translate m ast _ (Fork _ e) = do
--   (h1, _) <- translate m ast IOType e
--   return ("_fork (" ++ h1 ++ " >> return ())", IOType)

-- -- Session types
-- translate _ _ _ (New _ _) = return ("_new", IOType)
-- translate m ast t (Send _ e) = do
--   (h1, t1) <- translate m ast PureType e
--   if isIO t1 then do
--     f <- nextFresh
--     return (h1 ++ " >>= \\" ++ f ++ " -> (_send " ++ f ++ ")", IOType) 
--   else  
--     return ("(_send " ++ h1 ++ ")", IOType)

-- translate m ast t (Receive _ e) = do
--   (h1, t1) <- translate m ast PureType e
--   if isIO t1 then do
--     f <- nextFresh
--     return (h1 ++ " >>= \\" ++ f ++ " -> (_receive " ++ f ++ ")", IOType) 
--   else  
--     return ("(_receive " ++ h1 ++ ")", IOType)

-- translate m ast nt e@(Select _ e1 x) = do
--   (h, _) <- translate m ast PureType e1 -- PureType or nt??
--   return ("(_send " ++ h ++ " \"" ++ show x ++ "\")", IOType)
  
-- translate m ast nt (Match _ e mm) = do
--   (h1, t1) <- translate m ast PureType e 
--   v <- nextFresh
--   fresh <- nextFresh
-- --  traceM ("NT:" ++ show nt)
--   (h2, t2) <- translateMatchMap fresh m ast nt mm  
--   return ("_receive " ++ h1 ++ " >>= \\(" ++ v ++  ", " ++ fresh ++
--           ") -> (case " ++ v ++ " of {" ++ h2 ++ "})", max t1 t2)   
  
-- typeMatch :: NodeType -> NodeType -> NodeType
-- typeMatch (ArrowType s1 s2) (ArrowType s3 s4) = ArrowType (max s1 s3) (typeMatch s2 s4)
-- typeMatch t1 t2 = max t1 t2

-- translateCaseMap :: AnnFunMap -> AST -> NodeType -> FieldMap -> TranslateM (String, NodeType)
-- translateCaseMap m ast nt =
--   Map.foldlWithKey translateCaseMap' (return ("", PureType))
--   where
--     translateCaseMap' :: TranslateM (String, NodeType) -> ProgVar ->
--                          ([ProgVar], Expression) -> TranslateM (String, NodeType)
--     translateCaseMap' acc cons (ps, e) = do
--       (h, n1) <- translate m ast nt e
--       (acc', n2) <- acc
--       return (acc' ++ "\n    " ++ show cons ++ showCaseParams ps ++ " -> " ++ h ++ ";",
--               max n1 n2)

--     showCaseParams :: [ProgVar] -> String
--     showCaseParams [] = ""
--     showCaseParams args = foldl (\acc a -> acc ++ " " ++ show a) "" args


-- translateMatchMap :: String -> AnnFunMap -> AST -> NodeType -> FieldMap -> TranslateM (String, NodeType)
-- translateMatchMap fresh m ast nt =
--   Map.foldlWithKey (translateMatchMap' fresh) (return ("", PureType))
--   where
--     translateMatchMap' :: String -> TranslateM (String, NodeType) -> ProgVar ->
--                          ([ProgVar], Expression) -> TranslateM (String, NodeType)
--     translateMatchMap' fresh acc v (p:_, e) = do
--       (h, n1) <- translate m ast nt e
--       (acc', n2) <- acc
--       return (acc' ++ "\n    \"" ++ show v ++ "\" " ++
--         " -> let " ++ show p ++ " = " ++ fresh ++ " in " ++ h ++ ";", max n1 n2)
             


-- -- TODO: change name
-- applyNodeType :: NodeType -> NodeType -> NodeType
-- applyNodeType t1 t2 = normaliseType (applyNodeType' t1 t2)

-- applyNodeType' :: NodeType -> NodeType -> NodeType
-- applyNodeType' a@(ArrowType _ _) (ArrowType b@(ArrowType _ _) s4) = ArrowType (applyNodeType' a b) s4
-- --applyNodeType' (ArrowType  a@(ArrowType _ _) s1) b@(ArrowType _ _) = ArrowType (applyNodeType' b a) s1
-- --applyNodeType' (ArrowType s1 s2) (ArrowType s3 s4) = ArrowType (applyNodeType' s1 s3) (applyNodeType' s2 s4)2
-- applyNodeType' (ArrowType _ s2)  t                 = ArrowType t s2
-- applyNodeType' t                 u                 = max t u

-- normaliseType :: NodeType -> NodeType
-- normaliseType t
--   | anyIO t    = updateLastType IOType t 
--   | otherwise = t

-- anyIO :: NodeType -> Bool
-- anyIO (ArrowType s1 s2) = anyIO s1 || anyIO s2
-- anyIO IOType           = True
-- anyIO _                = False

-- zipFromEnd :: NodeType -> NodeType -> NodeType
-- zipFromEnd t1 t2 = 
--   foldr (\t acc -> ArrowType t acc) t2
--     (nodeTypeTake (nodeTypeLength t1 - nodeTypeLength t2) t1)

-- nodeTypeLength :: NodeType -> Int
-- -- nodeTypeLength (ArrowType t1 t2) = nodeTypeLength t1 + nodeTypeLength t2
-- nodeTypeLength (ArrowType t1 t2) = 1 + nodeTypeLength t2
-- nodeTypeLength t                 = 1

-- nodeTypeTake :: Int -> NodeType -> [NodeType]
-- nodeTypeTake 0 _ = []
-- nodeTypeTake n (ArrowType t1 t2) = t1 : nodeTypeTake (n-1) t2
-- nodeTypeTake _ t = [t]

-- -- TODO: Monad version, uncomment when annExps turns monad
-- -- updateProgVar :: Expression -> NodeType -> TranslateM ()
-- -- updateProgVar (App _ e _) t = updateProgVar e t
-- -- updateProgVar e@(ProgVar _ _) t1 = do
-- --    t2 <- getFromAST e
-- --    insertAST e (zipFromEnd t2 t1)

-- updateProgVar :: AST -> Expression -> NodeType -> AST
-- updateProgVar m (App _ e _) t = updateProgVar m e t
-- updateProgVar m e@(ProgVar _ _) t1 =
--    let t2 = m Map.! e in
--    Map.insert e (zipFromEnd t2 t1) m 
-- updateProgVar m e@(TypeApp _ _ _) t1 =
--    let t2 = m Map.! e in
--    -- trace ("\nUPDATE TYPE APP : " ++ show e ++
--    --        "\n to update Type " ++ show t1 ++
--    --        "\n astType " ++ show t2 ++
--    --        "\n zipedType " ++ show (zipFromEnd t2 t1) ++ "\n")
--    Map.insert e (zipFromEnd t2 t1) m
-- updateProgVar m _ _ = m
  
-- -- TODO: Check if need to remove the other last type, it makes sense?
-- -- if so, rename this into lastType
-- lType :: NodeType -> NodeType
-- lType (ArrowType _ t) = t
-- lType t               = t

-- -- TODO: Think about alternatives to this problem
-- updateEEnv :: ExpEnv -> ExpEnv
-- updateEEnv = Map.foldrWithKey (\f e acc -> Map.insert f (updateEEnv' e) acc) Map.empty

-- updateEEnv' :: Expression -> Expression
-- updateEEnv' e@(ProgVar p x) =
--   if show x == "main" then (ProgVar p (mkVar p "_main"))
--   else e

-- updateEEnv' (Lambda p m x t e) = Lambda p m x t (updateEEnv' e)
-- updateEEnv' (App p e1 e2) = (App p (updateEEnv' e1) (updateEEnv' e2))
-- updateEEnv' (Pair p e1 e2) = (Pair p (updateEEnv' e1) (updateEEnv' e2))
-- updateEEnv' (BinLet p x y e1 e2) = (BinLet p x y (updateEEnv' e1) (updateEEnv' e2))
-- -- updateEEnv' (Case Pos Expression FieldMap

-- updateEEnv' (Conditional p e1 e2 e3) = (Conditional p (updateEEnv' e1) (updateEEnv' e2) (updateEEnv' e3))
-- updateEEnv' (UnLet p x e1 e2) = (UnLet p x (updateEEnv' e1) (updateEEnv' e2))
-- updateEEnv' (Fork p e) = (Fork p (updateEEnv' e))
-- updateEEnv' (Send p e) = (Send p (updateEEnv' e))
-- updateEEnv' (Receive p e) = (Receive p (updateEEnv' e))
-- updateEEnv' (Select p e x) = (Select p (updateEEnv' e) x)
-- -- updateEEnv' (Match Pos Expression FieldMap
-- updateEEnv' e = e
