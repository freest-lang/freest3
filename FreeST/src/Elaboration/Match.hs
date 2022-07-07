module Elaboration.Match
  ( matchFuns
  )
where

import           Data.List(groupBy,sortOn)
import           Data.Function((&))
import           Data.Functor((<&>))
import qualified Data.Map.Strict   as Map

import           Syntax.Base
import           Syntax.Expression
import qualified Syntax.Type       as T
import qualified Validation.Rename as R
import           Parse.ParseUtils(binOp)

import           Util.FreestState

import           Debug.Trace -- debug (used on debugM function)

matchFuns :: ParseEnvP -> FreestState ParseEnv
matchFuns pep = mapM matchFun pep

matchFun :: [([Pattern],Exp)] -> FreestState ([Variable],Exp)
matchFun xs@((ps,_):_) = mapM newVar ps
                     >>= \args -> (,) args <$> match args xs

match :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
match vs x
  | isRuleLit   x = ruleLit   vs x
  | isRuleEmpty x = ruleEmpty vs x
  | isRuleVar   x = ruleVar   vs x
  | isRuleCon   x = ruleCon   vs x
  | otherwise     = ruleMix   vs x

-- is rule ---------------------------------------------------------
isRuleLit   :: [([Pattern],Exp)] -> Bool
isRuleLit cs = any (check.fst) cs
  where check p = not   (null p)
               && isLit (head p)

isRuleEmpty :: [([Pattern],Exp)] -> Bool
isRuleEmpty cs = all (null.fst) cs

isRuleVar   :: [([Pattern],Exp)] -> Bool
isRuleVar cs = all (check.fst) cs
  where check p = not   (null p)
               && isVar (head p)

isRuleCon   :: [([Pattern],Exp)] -> Bool
isRuleCon cs = all (check.fst) cs
  where check p = not   (null p)
               && isCon (head p)

-- rules -----------------------------------------------------------

-- empty -----------------------------------------------------------
ruleEmpty :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleEmpty _ [] = return $ Int defaultSpan (-1) -- TODOX use freest undefined 
ruleEmpty _ ((_,e):cs) = replaceExp v v e
  where v = mkVar (defaultSpan) "__"

-- var -------------------------------------------------------------
ruleVar :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleVar (v:us) cs = match us =<< (mapM replace cs)
  where replace (p:ps,e)
          | is_ p     = return (ps,e)
          | otherwise = (,) ps <$> (replaceExp v (pVar p) e)

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
ruleMix (v:us) cs = do
  cons <- constructors $ getDataType cs
  groupOn (isVar.head.fst) cs
        & mapM (fill v cons)
      <&> concat
      >>= match (v:us)

--rule mix aux
fill :: Variable -> [(Variable,Int)] -> [([Pattern],Exp)] -> FreestState [([Pattern],Exp)]
fill v cons cs 
  | hasVar cs = fill' v cons cs
  | otherwise = return cs
  where hasVar = isVar.head.fst.head

fill' :: Variable -> [(Variable,Int)] -> [([Pattern],Exp)] -> FreestState [([Pattern],Exp)]
fill' v _ [] = return []
fill' v cons ((p:ps,e):cs) = (++) <$> mapM (mkCons v' e') cons <*> fill' v cons cs
  where v' = V $ mkVar (getSpan $ pVar p) "_"
        e' = replaceExp v (pVar p) e
        mkCons v e' (c,n) = (,) (C c (replicate n v):ps) <$> e'
  
-- gets the first contructor name
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
getKeys _ = []

-- lit -------------------------------------------------------------
ruleLit :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleLit vs cs = do
  -- ifs   -> vars until the first lit
  -- elses -> everything else after the first lit
  let (ifs,elses1) = span (not.isLit.head.fst) cs
  let (p,elses2) = (head elses1, tail elses1)
  let lit = head $ fst $ p
  --- same literals and other vars
  let ifRest1      = p : filter ((isGroup  lit).head.fst) elses2
  -- dif literals and other vars
  let elseRest    =       filter ((notGroup lit).head.fst) elses2
  -- transform lits into vars
  let ifRest2      = map (litToVar) ifRest1
  -- if cond then group1 else group 2
  let group1 = ifs++ifRest2
  let group2 = ifs++elseRest
  let cond = comp (head vs) $ pLit lit
  -- 
  g1 <- match vs group1
  g2 <- match vs group2
  return $ Cond (getSpan $ pLit lit ) cond g1 g2

litToVar :: ([Pattern],Exp) -> ([Pattern],Exp) 
litToVar (((L e):ps),exp) = (((V $ mkVar (getSpan e) "_"):ps),exp)
litToVar p = p

isGroup :: Pattern -> Pattern -> Bool
isGroup _        (V _)  = True
isGroup (L e1) (L e2) = sameLit' e1 e2

notGroup :: Pattern -> Pattern -> Bool
notGroup _      (V _)  = True
notGroup (L e1) (L e2) = not $ sameLit' e1 e2

sameLit' :: Exp -> Exp -> Bool
sameLit' (Int    _ k1) (Int    _ k2) = k1 == k2
sameLit' (Char   _ k1) (Char   _ k2) = k1 == k2
sameLit' (Bool   _ k1) (Bool   _ k2) = k1 == k2
sameLit' (String _ k1) (String _ k2) = k1 == k2
sameLit' _               _               = False

-- TODOX
comp :: Variable -> Exp -> Exp
comp v i@(Int    s k) = binOp (Var (getSpan v) v) (mkVar s "(==)") i
-- comp v c@(E.Char   s k) = 
-- comp v b@(E.Bool   s k) = 
-- comp v s@(E.String s k) = 
comp _ e = Bool (getSpan e) False

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
replaceExp v p (CaseP s e flp)        = sub         <$> replaceExp  v p e  <*>(replaceExp v p    =<< match vs' flp)
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
pVar (L e)   = mkVar (getSpan e) (show e)

pPats :: Pattern -> [Pattern]
pPats (C _ ps) = ps

pLit :: Pattern -> Exp
pLit (L e) = e

isVar :: Pattern -> Bool
isVar (V _) = True
isVar _     = False

isCon :: Pattern -> Bool
isCon (C _ _) = True
isCon _       = False

isLit :: Pattern -> Bool
isLit (L _) = True
isLit _     = False

is_ :: Pattern -> Bool
is_ (V v) = intern v == "_"

newVar :: Pattern -> FreestState Variable
newVar = R.renameVar.pVar

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy apply
  where apply n1 n2 = f n1 == f n2

groupSortBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupOn f . sortOn f
