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
initialEnv venv = Map.foldrWithKey (\k t m -> Map.insert k (funToArrow t) m) initialAcc varEnv
  where initialAcc =
          Map.fromList [ (mkVar defaultPos "printInt", ArrowType PureType IOType)
                       , (mkVar defaultPos "printBool", ArrowType PureType IOType)
                       , (mkVar defaultPos "printChar", ArrowType PureType IOType)
                       , (mkVar defaultPos "printUnit", ArrowType PureType IOType)
                       ] 
        varEnv = foldr (\k acc -> Map.delete k acc) venv 
                      [(mkVar defaultPos "printInt"),
                       (mkVar defaultPos "printBool"),
                       (mkVar defaultPos "printChar"),
                       (mkVar defaultPos "printUnit")] 
                           -- (mkVar defaultPos "printValue") venv
        
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
  ArrowType (updateLastType exp a) (updateLastType exp t2)            
--  ArrowType (updateLastType (checkType f) a) (updateLastType exp t2)            
updateLT _ exp (ArrowType t1 t2)  = 
  ArrowType t1 (updateLastType exp t2)
updateLT _ exp t = max exp t

updateLastType :: NodeType -> NodeType -> NodeType
updateLastType exp (ArrowType t1 t2)  = 
  ArrowType t1 (updateLastType exp t2)
updateLastType exp t = max exp t        

annFun :: AnnFunMap -> Expression -> NodeType
annFun _ (Unit _) = PureType
annFun _ (Integer _ _) = PureType
annFun _ (Character _ _) = PureType
annFun _ (Boolean _ _) = PureType
annFun m (ProgVar _ x) = annVar m x
annFun m (Lambda _ _ _ _ e) = annFun m e
annFun m (App _ e1 e2) = max (annFun m e1) (annFun m e2)
annFun m (TypeApp _ x _) = annVar m x
annFun m (Pair _ _ e1 e2) = max (annFun m e1) (annFun m e2)
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
    let funMap = fm in (funMap, annotateAST funMap Map.empty eenv)
  where fm = top eenv tenv venv
  
-- -- type ExpEnv = Map.Map ProgVar Expression
-- findExpFixedPoint :: AnnFunMap -> ExpEnv -> AST -> AST
-- findExpFixedPoint m eenv ast
--   | ast == ast' = ast
--   | otherwise = findExpFixedPoint m eenv ast'
--   where ast' = annotateAST m ast eenv

-- annotateAST m = Map.foldrWithKey (\f e ast -> fst $ annExp m ast (nodeType f)  e)
annotateAST :: AnnFunMap -> AST -> ExpEnv -> AST
annotateAST m = Map.foldrWithKey (\f e ast -> fst $ annExp m ast (nodeType f) e)
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
      (Map.insert e t1 ast, t1)
-- Abstraction intro and elim  
annExp fm ast t e@(Lambda _ _ x _ e1) =
  let (ast1, t1) = annExp fm ast t e1
      maxt       = ArrowType PureType (max t t1) in
    (Map.insert e maxt ast1, maxt) -- TODO: max??

annExp fm ast t e@(App _ e1 e2) =
  let (ast1, t1) = annExp fm ast t e1
      (ast2, t2) = annExp fm ast1 PureType e2
      t3         = applyNodeType (max t2 t1) (min t2 t1) -- max and min ... should be like this?
      ast3       = updateProgVar ast2 e1 (updateLastType (max (lastType t) (lastType t3)) t3) in
      (Map.insert e (lastType t3) ast3, lType t3)
      
-- Pair intro and elim    
annExp fm ast t e@(Pair _ _ e1 e2) =
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
    (Map.insert e IOType ast1, IOType)    
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
