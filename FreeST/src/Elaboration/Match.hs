module Elaboration.Match
  ( matchFuns
  )
where

import           Data.List            (groupBy,sortOn,transpose)
import           Data.Function        ((&))
import           Data.Functor         ((<&>))
import           Control.Monad        (filterM)
import           Control.Monad.Extra  ((&&^),findM)
import           Control.Bool         (ifThenElseM)

import           Syntax.Base
import           Syntax.Expression
import qualified Syntax.Type       as T
import qualified Validation.Rename as R
import qualified Data.Map.Strict   as Map

import           Util.FreestState

import           Debug.Trace -- debug (used on debugM function)


-- match -----------------------------------------------------------

type Equation = ([Pattern],Exp)

matchFuns :: ParseEnvPat -> FreestState ParseEnv
matchFuns pep = mapM matchFun pep

matchFun :: [Equation] -> FreestState ([Variable],Exp)
matchFun xs@((ps,_):_) = mapM newVar ps
                     >>= \args -> (,) args <$> match args xs

match :: [Variable] -> [Equation] -> FreestState Exp
match vs x = do
  ifThenElseM (isRuleChan  x) 
              (ruleChan vs x) (match' vs x)

match' :: [Variable] -> [Equation] -> FreestState Exp
match' vs x
  | isRuleEmpty x = ruleEmpty vs x
  | isRuleVar   x = ruleVar   vs x
  | isRuleCon   x = ruleCon   vs x
  | otherwise     = ruleMix   vs x

-- is rule ---------------------------------------------------------
isRuleEmpty :: [Equation] -> Bool
isRuleEmpty cs = all (null.fst) cs

isRuleVar   :: [Equation] -> Bool
isRuleVar cs = all (check.fst) cs
  where check p = not   (null p)
               && isVar (head p)

isRuleCon   :: [Equation] -> Bool
isRuleCon cs = all (check.fst) cs
  where check p = not   (null p)
               && isCon (head p)

isRuleChan  :: [Equation] -> FreestState Bool
isRuleChan cs = b1 &&^ b2
  where b1 = return $ (not $ isRuleEmpty cs) && (not $ isRuleVar cs) && (isRuleCon cs)
        b2 = and <$> mapM (isChan.head.fst) cs

-- rules -----------------------------------------------------------

-- empty -----------------------------------------------------------
ruleEmpty :: [Variable] -> [Equation] -> FreestState Exp
ruleEmpty _ ((_,e):cs) = do v' <- v; replaceExp v' v' e
  where v = R.renameVar $ mkVar (defaultSpan) "_"

-- var -------------------------------------------------------------
ruleVar :: [Variable] -> [Equation] -> FreestState Exp
ruleVar (v:us) cs = match us =<< (mapM replace cs)
  where replace (p:ps,e)
          | isPat_ p     = return (ps,e)
          | otherwise = (,) ps <$> (replaceExp v (pVar p) e)

-- con -------------------------------------------------------------
ruleCon :: [Variable] -> [Equation] -> FreestState Exp
ruleCon (v:us) cs = groupSortBy (pName.head.fst) cs
                  & mapM destruct
                >>= mapM (\(con,vs,cs) -> (,) con . (,) vs <$> match (vs++us) cs)
                <&> Case s (Var s v) . Map.fromList
  where s = getSpan v
  
-- rule con aux 
destruct :: [Equation] -> FreestState (Variable, [Variable], [Equation])
destruct l@((p:ps,_):cs) = mapM newVar (pPats p)
                       <&> flip ((,,) (pVar p)) l'
  where l' = map (\(p:ps,e) -> ((pPats p)++ps,e)) l

-- chan ------------------------------------------------------------
ruleChan :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleChan (v:us) cs = groupSortBy (pName.head.fst) cs
                   & mapM destruct
                 >>= mapM (\(con,vs,cs) -> (,) con . (,) vs <$> match (vs++us) cs)
                 <&> Case s (App s (Var s (mkVar s "collect")) (Var s v)) . Map.fromList
  where s = getSpan v

-- mix -------------------------------------------------------------
ruleMix :: [Variable] -> [Equation] -> FreestState Exp
ruleMix (v:us) cs = do
  cons <- getFiller cs
  groupOn (isVar.head.fst) cs
        & mapM (fill v cons)
      <&> concat
      >>= match (v:us)

getFiller :: [Equation] -> FreestState [(Variable,Int)]
getFiller cs = do
  c <- findM (isChan.head.fst) cs
  case c of 
    Nothing -> constructors $ getDataType cs
    Just (p:_,_) -> getPEnvChoices
                <&> flip (Map.!) (pVar p)
                <&> \vs -> zip vs (replicate (length vs) 1)

--rule mix aux
fill :: Variable -> [(Variable,Int)] -> [Equation] -> FreestState [Equation]
fill v cons cs 
  | hasVar cs = fill' v cons cs
  | otherwise = return cs
  where hasVar = isVar.head.fst.head

fill' :: Variable -> [(Variable,Int)] -> [Equation] -> FreestState [Equation]
fill' v _ [] = return []
fill' v cons ((p:ps,e):cs) = (++) <$> mapM (mkCons v' e') cons <*> fill' v cons cs
  where v' = PatVar $ mkVar (getSpan $ pVar p) "_"
        e' = replaceExp v (pVar p) e
        mkCons v e' (c,n) = (,) (PatCons c (replicate n v):ps) <$> e'
  
-- gets the first contructor name
getDataType :: [([Pattern], Exp)] -> Variable
getDataType cs = filter (isCon.head.fst) cs
               & pVar.head.fst.head

-- returns constructors and the amount of variables they need
constructors :: Variable -> FreestState [(Variable,Int)]
constructors c = (findCons c).(map (snd.snd)).(Map.toList) <$> getTEnv

findCons :: Variable -> [T.Type] -> [(Variable,Int)]
findCons c [] = []
findCons c (t:ts) 
  | c `elem` getKeys t = constructors' t
  | otherwise = findCons c ts

constructors' :: T.Type -> [(Variable,Int)]
constructors' (T.Almanac _ T.Variant tm) = map (\(v,t) -> (v,countArrows t)) (Map.toList tm)
  where countArrows (T.Arrow _ _ _ t2) = 1 + countArrows t2
        countArrows _ = 0 

getKeys :: T.Type -> [Variable]
getKeys (T.Almanac _ T.Variant tm) = Map.keys tm
getKeys _ = []

-- replace Variables -----------------------------------------------
replaceExp :: Variable -> Variable -> Exp -> FreestState Exp
replaceExp v p (Var     s v1)         = Var     s      (replaceVar v p v1) & return
replaceExp v p (Abs     s m b)        = Abs     s m <$> replaceBind v p b
replaceExp v p (App     s e1 e2)      = App     s   <$> replaceExp  v p e1 <*> replaceExp v p e2
replaceExp v p (Pair s e1 e2)         = Pair    s   <$> replaceExp  v p e1 <*> replaceExp v p e2
replaceExp v p (BinLet s v1 v2 e1 e2) = BinLet  s      (replaceVar  v p v1)   (replaceVar v p v2)
                                                    <$> replaceExp  v p e1 <*> replaceExp v p e2
replaceExp v p (Case s e fm)          = Case    s   <$> replaceExp  v p e  <*> mapM (substitute v p) fm
replaceExp v p (TypeAbs s b)          = TypeAbs s   <$> replaceBind v p b
replaceExp v p (TypeApp s e t)        = flip (TypeApp s) t <$> replaceExp v p e
replaceExp v p (Cond s e1 e2 e3)      = Cond    s   <$> replaceExp  v p e1 <*> replaceExp v p e2 <*> replaceExp v p e3
replaceExp v p (UnLet s v1 e1 e2)     = UnLet   s      (replaceVar  v p v1)<$> replaceExp v p e1 <*> replaceExp v p e2
replaceExp v p (CasePat s e flp)      = sub         <$> replaceExp  v p e  <*>(replaceExp v p    =<< match vs' flp)
  where sub e (Case s _ fm) = Case s e fm
        vs' = [mkVar (getSpan e) "_"]
replaceExp _ _ e = return e

replaceBind :: Variable -> Variable -> Bind a Exp -> FreestState (Bind a Exp)
replaceBind v p b@(Bind {var=v1,body=exp}) = replaceExp v p exp
                                         <&> (\e -> b {var=replaceVar v p v1,body=e})

replaceVar :: Variable -> Variable -> Variable -> Variable
replaceVar (Variable _ name) (Variable _ name1) v@(Variable span name2)
  | name1 == name2 = Variable span name
  | otherwise      = v

substitute :: Variable -> Variable -> ([Variable],Exp) -> FreestState ([Variable],Exp)
substitute v p (vs,e) = (,) (map (replaceVar v p) vs) <$> (replaceExp v p e)

-- aux
pName :: Pattern -> String
pName (PatCons v _) = intern v

pVar :: Pattern -> Variable
pVar (PatVar  v)   = v
pVar (PatCons v _) = v

pPats :: Pattern -> [Pattern]
pPats (PatCons _ ps) = ps

isVar :: Pattern -> Bool
isVar (PatVar _) = True
isVar _          = False

isCon :: Pattern -> Bool
isCon = not.isVar

isChan :: Pattern -> FreestState Bool
isChan (PatVar _)    = return False
isChan (PatCons c _) = Map.toList <$> getTEnv
             <&> map (getKeys.snd.snd)
             <&> concat
             <&> notElem c

isPat_ :: Pattern -> Bool
isPat_ (PatVar v) = is_ v

newVar :: Pattern -> FreestState Variable
newVar = R.renameVar.pVar

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy apply
  where apply n1 n2 = f n1 == f n2

groupSortBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupOn f . sortOn f
