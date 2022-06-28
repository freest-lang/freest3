module Elaboration.Match
  ( matchFuns
  )
where

import           Data.List(groupBy,sortOn)
import           Data.Function((&))
import           Data.Functor((<&>))
import           Data.Traversable
import           Control.Monad

import           Syntax.Base
import           Syntax.Expression
import qualified Syntax.Type       as T
import qualified Validation.Rename as R
import qualified Data.Map.Strict   as Map

import           Util.FreestState

matchFuns :: ParseEnvP -> FreestState ParseEnv
matchFuns pep = mapM matchFun pep

matchFun :: [([Pattern],Exp)] -> FreestState ([Variable],Exp)
matchFun xs@((ps,_):_) = mapM newVar ps
                     >>= \args -> (,) args <$> match args xs

match :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
match vs x
  | isRuleEmpty x = ruleEmpty vs x
  | isRuleVar   x = ruleVar   vs x
  | isRuleCon   x = ruleCon   vs x
  | otherwise     = ruleMix   vs x

-- is rule ---------------------------------------------------------
isRuleEmpty :: [([Pattern],Exp)] -> Bool
isRuleEmpty cs = and $ map (null.fst) cs

isRuleVar   :: [([Pattern],Exp)] -> Bool
isRuleVar cs = and $ map (check.fst) cs
  where check p = not   (null p)
               && isVar (head p)

isRuleCon   :: [([Pattern],Exp)] -> Bool
isRuleCon cs = and $ map (check.fst) cs
  where check p = not   (null p)
               && isCon (head p)

-- rules -----------------------------------------------------------

-- empty -----------------------------------------------------------
ruleEmpty :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleEmpty _ ((_,e):cs) = replaceExp v v e
  where v = mkVar (defaultSpan) "__"

-- var -------------------------------------------------------------
ruleVar :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleVar (v:us) cs = match us =<< (mapM replace cs)
  where replace (p:ps,e) = (,) ps <$> (replaceExp v (pVar p) e)

-- con -------------------------------------------------------------
ruleCon :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleCon (v:us) cs = groupSortBy (pName.head.fst) cs
                  & mapM destruct
                >>= mapM (\(con,vs,cs) -> (,) con . (,) vs <$> match (vs++us) cs)
                <&> Case s (Var s v) . Map.fromList
  where s = getSpan v
  
-- rule con aux 
destruct :: [([Pattern],Exp)] -> FreestState (Variable, [Variable], [([Pattern],Exp)])
destruct l@((p:ps,_):cs) = mapM newVar (pPats p)
                       <&> (\args -> (,,) (pVar p) args (destruct' l))

destruct' :: [([Pattern],Exp)] -> [([Pattern],Exp)]
destruct' [] = []
destruct' ((p:ps,e):xs) = ((pPats p)++ps,e) : destruct' xs 

-- mix -------------------------------------------------------------
ruleMix :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleMix us cs = do
  cons <- constructors $ getDataType cs
  match us $ groupOn (isVar.head.fst) cs
           & concat.map (fill cons)

--rule mix aux
fill :: [(Variable,Int)] -> [([Pattern],Exp)] -> [([Pattern],Exp)]
fill cons cs 
  | hasVar cs = fill' cons cs
  | otherwise = cs
  where hasVar = isVar.head.fst.head

fill' :: [(Variable,Int)] -> [([Pattern],Exp)] -> [([Pattern],Exp)]
fill' _ [] = []
fill' cons ((p:ps,e):cs) = map mkCons cons ++ fill' cons cs
  where v = V $ mkVar (getSpan $ pVar p) "_"
        mkCons (c,n) = ((C c (replicate n v):ps),e)

-- gets the data type from the constructor
getDataType :: [([Pattern], Exp)] -> Variable
getDataType cs = filter (isCon.head.fst) cs
               & pVar.head.fst.head

-- returns constructors and the amount of variables they need
constructors :: Variable -> FreestState [(Variable,Int)]
constructors c = findDt.Map.toList <$> getTEnv
  where findDt ((dt,(_,t)):xs) = 
          if c `elem` getKeys t then constructors' t else findDt xs

constructors' :: T.Type -> [(Variable,Int)]
constructors' (T.Almanac _ T.Variant tm) = map (\(v,t) -> (v,countArrows t)) (Map.toList tm)
  where countArrows (T.Arrow _ _ _ t2) = 1 + countArrows t2
        countArrows _ = 0 

getKeys :: T.Type -> [Variable]
getKeys (T.Almanac _ T.Variant tm) = Map.keys tm

-- replace Variables -----------------------------------------------
replaceExp :: Variable -> Variable -> Exp -> FreestState Exp
replaceExp v p (Var     s v1)         = Var s (replaceVar v p v1) & return
replaceExp v p (Abs     s m b)        = Abs     s m <$> replaceBind v p b
replaceExp v p (App     s e1 e2)      = App     s   <$> replaceExp v p e1 <*> replaceExp v p e2
replaceExp v p (Pair s e1 e2)         = Pair    s   <$> replaceExp v p e1 <*> replaceExp v p e2
replaceExp v p (BinLet s v1 v2 e1 e2) = BinLet  s (replaceVar v p v1) (replaceVar v p v2) <$> replaceExp v p e1 <*> replaceExp v p e2
replaceExp v p (Case s e fm)          = Case    s <$> replaceExp v p e <*> mapM (substitute v p) fm
replaceExp v p (TypeAbs s b)          = TypeAbs s <$> replaceBind v p b
replaceExp v p (TypeApp s e t)        = flip (TypeApp s) t <$> replaceExp v p e
replaceExp v p (Cond s e1 e2 e3)      = Cond    s <$> replaceExp v p e1 <*> replaceExp v p e2 <*> replaceExp v p e3
replaceExp v p (UnLet s v1 e1 e2)     = UnLet   s (replaceVar v p v1) <$> replaceExp v p e1 <*> replaceExp v p e2
replaceExp v p (CaseP s e flp)        = sub <$> replaceExp v p e <*> match vs' flp >>= replaceExp v p
  where sub e (Case s _ fm) = Case s e fm
        vs' = [mkVar (getSpan e) "_"]
replaceExp _ _ e = return e

replaceBind :: Variable -> Variable -> Bind a Exp -> FreestState (Bind a Exp)
replaceBind v p b@(Bind {var=v1,body=exp}) = (\e -> b {var=replaceVar v p v1,body=e}) <$> replaceExp v p exp

replaceVar :: Variable -> Variable -> Variable -> Variable
replaceVar (Variable _ name) (Variable _ name1) v@(Variable span name2)
  | name1 == name2 = Variable span name
  | otherwise      = v

substitute :: Variable -> Variable -> ([Variable],Exp) -> FreestState ([Variable],Exp)
substitute v p (vs,e) = (,) (map (replaceVar v p) vs) <$> (replaceExp v p e)

-- aux
pName :: Pattern -> String
pName (C v _) = intern v

pVar :: Pattern -> Variable
pVar (V v)   = v
pVar (C v _) = v

pPats :: Pattern -> [Pattern]
pPats (C _ ps) = ps

isVar :: Pattern -> Bool
isVar (V _) = True
isVar _     = False

isCon :: Pattern -> Bool
isCon = not.isVar

newVar :: Pattern -> FreestState Variable
newVar = R.renameVar.pVar

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy apply
  where apply n1 n2 = f n1 == f n2

groupSortBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupOn f . sortOn f
