module Elaboration.Match3
  ( matchFun
  )
where

import           Data.List(groupBy,sortOn)
import           Data.Function((&))
import           Data.Traversable
import           Control.Monad

import           Syntax.Base
import           Syntax.Expression
import qualified Syntax.Type       as T
import qualified Validation.Rename as R
import qualified Data.Map.Strict   as Map

import           Util.FreestState

-- TODO remove
import           Debug.Trace

--------------- just to remember the format ----------------
-- cases                                                  --
-- type FieldMap  = Map.Map Variable ([Variable], Exp)    --
-- type FieldMapP = Map.Map Variable [([Pattern], Exp)]   --
-- functions                                              --
-- type ParseEnv  = Map.Map Variable ([Variable], Exp)    --
-- type ParseEnvP = Map.Map Variable [([Pattern], Exp)]   --
------------------------------------------------------------

------------------------------------------------------------
-- instance Show Pattern where
--   show (V v)    = "Var " ++ show v
--   show (C v xs) = "Pat " ++ show v ++ " "++ show xs ++" "
------------------------------------------------------------

matchFun :: ParseEnvP -> FreestState ParseEnv
matchFun pep = mapM match pep

match :: [([Pattern],Exp)] -> FreestState ([Variable],Exp)
match xs@(x:_) = do 
  arguments <- mapM newVar (fst x)
  result <- matching arguments xs
  return $ (arguments, result)

matching :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
matching vs x
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
ruleEmpty _ ((_,e):cs) = return e

-- var -------------------------------------------------------------
ruleVar :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleVar (v:us) cs = matching us (map replace cs)
  where replace (p:ps,e) = (ps, replaceExp v (pVar p) e)

-- con -------------------------------------------------------------
ruleCon :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleCon (v:us) cs = do
  let a = groupSortBy (name.head.fst) cs
  b <- mapM destruct a
  let s = getSpan v
  cs' <- mapM (\(con,vs,cs) -> return.(,) con.(,) vs =<< matching (vs++us) cs) b
  let c = Case s (Var s v) (Map.fromList cs')
  return c

-- rule con aux 
destruct :: [([Pattern],Exp)] -> FreestState (Variable, [Variable], [([Pattern],Exp)])
destruct l@((p:ps,_):cs) = construct (pVar p) (destruct' l) =<< newArgs
  where newArgs = mapM newVar (pPats p)
        construct a c b = return (a,b,c)

destruct' :: [([Pattern],Exp)] -> [([Pattern],Exp)]
destruct' [] = []
destruct' ((p:ps,e):xs) = ((pPats p)++ps,e) : destruct' xs 

-- mix -------------------------------------------------------------
ruleMix :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleMix us cs = do
  cons <- constructors $ getDataType cs
  let css = groupOn (isVar.head.fst) cs
  let cs' = concat $ map (fill cons) css
  matching us cs'

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

-- gets every constructor from the data type
getDataType :: [([Pattern], Exp)] -> Variable
getDataType cs = filter (isCon.head.fst) cs
               & pVar.head.fst.head

-- returns constructors and the amount of variables they need
constructors :: Variable -> FreestState [(Variable,Int)]
constructors c = return.findDt.Map.toList =<< getTEnv
  where findDt ((dt,(_,t)):xs) = 
          if c `elem` getKeys t then constructors' t else findDt xs

constructors' :: T.Type -> [(Variable,Int)]
constructors' (T.Almanac _ T.Variant tm) = map (\(v,t) -> (v,countArrows t)) (Map.toList tm)
  where countArrows (T.Arrow _ _ _ t2) = 1 + countArrows t2
        countArrows _ = 0 

getKeys :: T.Type -> [Variable]
getKeys (T.Almanac _ T.Variant tm) = Map.keys tm

-- replace Variables -----------------------------------------------
replaceExp :: Variable -> Variable -> Exp -> Exp
replaceExp v p (Var     s v1)          = Var     s (replaceVar v p v1)
replaceExp v p (Abs     s m b)         = Abs     s m (replaceBind v p b)
replaceExp v p (App     s e1 e2)       = App     s (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (Pair    s e1 e2)       = Pair    s (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (BinLet  s v1 v2 e1 e2) = BinLet  s (replaceVar v p v1) (replaceVar v p v2) (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (Case    s e fm)        = Case    s (replaceExp v p e) (Map.map (substitute v p) fm)
-- replaceExp v p (CaseP   s e fmp)       = CaseP   s (replaceExp v p e) (Map.map ((substitute v p).match) fmp) -- TODO monading
replaceExp v p (TypeAbs s b)           = TypeAbs s (replaceBind v p b)
replaceExp v p (TypeApp s e t)         = TypeApp s (replaceExp v p e) t
replaceExp v p (Cond    s e1 e2 e3)    = Cond    s (replaceExp v p e1) (replaceExp v p e2) (replaceExp v p e3)
replaceExp v p (UnLet   s v1 e1 e2)    = UnLet   s (replaceVar v p v1) (replaceExp v p e1) (replaceExp v p e2)
replaceExp _ _ e = e

-- TODO adicionar monad
-- separar monad do resto
-- fazer return ao resto

replaceBind :: Variable -> Variable -> Bind a Exp -> Bind a Exp
replaceBind v p (Bind {bSpan=s,var=v1,binder=t,body=exp}) =
  Bind {bSpan=s,var=replaceVar v p v1,binder=t,body=replaceExp v p exp}

replaceVar :: Variable -> Variable -> Variable -> Variable
replaceVar (Variable _ name) (Variable _ name1) v@(Variable span name2)
  | name1 == name2 = Variable span name
  | otherwise      = v

substitute :: Variable -> Variable -> ([Variable],Exp) -> ([Variable],Exp)
substitute v p (vs,e) = (map (replaceVar v p) vs, replaceExp v p e)

-- aux
name :: Pattern -> String
name (V v)   = ""
name (C v _) = intern v

pVar :: Pattern -> Variable
pVar (V v)   = v
pVar (C v _) = v

pPats :: Pattern -> [Pattern]
pPats (C _ ps) = ps

isVar :: Pattern -> Bool
isVar (V _) = True
isVar _     = False

isCon :: Pattern -> Bool
isCon (C _ _) = True
isCon _       = False

newVar :: Pattern -> FreestState Variable
newVar (V var)   = R.renameVar var
newVar (C var _) = R.renameVar var

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy apply
  where apply n1 n2 = f n1 == f n2

groupSortBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupOn f . sortOn f
