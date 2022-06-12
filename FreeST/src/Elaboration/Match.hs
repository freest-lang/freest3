module Elaboration.Match
  ( matchFun
  )
where

import           Data.List(groupBy,sortOn)
import           Data.Function((&))
import           Data.Traversable
import           Control.Monad

import           Syntax.Base
import           Syntax.Expression
import           Validation.Rename as R
import qualified Data.Map.Strict   as Map

import           Util.FreestState

--------------- just to remember the format ----------------
-- cases                                                  --
-- type FieldMap  = Map.Map Variable ([Variable], Exp)    --
-- type FieldMapP = Map.Map Variable [([Pattern], Exp)]   --
-- functions                                              --
-- type ParseEnv  = Map.Map Variable ([Variable], Exp)    --
-- type ParseEnvP = Map.Map Variable [([Pattern], Exp)]   --
------------------------------------------------------------

--                  fun args     patterns, exp   otherwise
data Match = M     [Variable] [([Pattern], Exp)] Match
--                 var        cons      vars      case
           | CaseM Variable [(Variable,[Variable],Match)]
--                 exp
           | ExpM  Exp
           | ERROR

matchFun :: ParseEnvP -> FreestState ParseEnv
matchFun pep = mapM match pep
-- matchFun pep = sequence $ Map.mapWithKey match pep

match :: [([Pattern],Exp)] -> FreestState ([Variable],Exp)
match xs@(x:_) = do 
  arguments <- mapM newVar (fst x)
  let m = M arguments xs ERROR 
  result <- matching m           
  return $ casefy arguments result

matching :: Match -> FreestState Match
matching (CaseM v xs) = return.CaseM v =<< (mapM f xs)
  where f (a,b,c) = matching c >>= return.(,,) a b
matching x@(M us cs o) 
  | isRuleEmpty x = return     $ ruleEmpty x
  | isRuleVar   x = matching   $ ruleVar x
  | isRuleCon   x = matching =<< ruleCon x
  | otherwise     = matching   $ ruleMix x
matching x = return x

casefy :: [Variable] -> Match -> ([Variable],Exp)
casefy as cs = (as, expfy cs)

expfy :: Match -> Exp
expfy (ExpM e)     = e
expfy (CaseM v cs) = Case s (Var s v) fm
  where s  = getSpan v
        fm = foldl (\m (con,vs,mat) -> Map.insert con (vs,expfy mat) m) Map.empty cs
-- expfy ERROR = ? TODO maybe should be added a monad for errors
-- expfy M not expected

-- is rule
isRuleEmpty :: Match -> Bool
isRuleEmpty (M _ cs _) = and $ map (null.fst) cs
isRuleEmpty _ = False

isRuleVar :: Match -> Bool
isRuleVar (M _ cs _) = and $ map (check.fst) cs
  where check p = not $ null p
               && isVar (head p)
isRuleVar _ = False

isRuleCon :: Match -> Bool
isRuleCon (M _ cs _) = and $ map (check.fst) cs
  where check p = not $ null p
               && isCon (head p)
isRuleCon _ = False

-- rules
ruleEmpty :: Match -> Match
ruleEmpty (M _ ((_,e):cs) _) = ExpM e

ruleVar :: Match -> Match
ruleVar (M (v:us) cs o) = M us cs' o
  where cs' = map (\(p:ps,e) -> (ps, replaceExp v (pVar p) e)) cs

ruleCon :: Match -> FreestState Match
ruleCon (M (v:us) cs o) = groupSortBy (name.head.fst) cs
                        & mapM destruct
                      >>= mapM (return.matchfy us o) -- ugly
                      >>= return.CaseM v
  -- do
  -- let a = groupSortBy (name.head.fst) cs
  -- b <- mapM destruct a
  -- let c = map (matchfy us o) b
  -- return $ CaseM v c
  
ruleMix :: Match -> Match
ruleMix (M us cs o) = groupOn (name.head.fst) cs
                    & distribute us o
  where distribute us o [] = o
        distribute us o (cs:css) = M us cs (distribute us o css)

-- rule con aux
destruct :: [([Pattern],Exp)] -> FreestState (Variable, [Variable], [([Pattern],Exp)])
destruct l@((p:ps,_):cs) = f =<< newArgs
  where newArgs = mapM newVar (pPats p)
        f a = return (pVar p,a,destruct' l)

destruct' :: [([Pattern],Exp)] -> [([Pattern],Exp)]
destruct' [] = []
destruct' ((p:ps,e):xs) = (ps'++ps,e) : destruct' xs 
  where ps' = pPats p

matchfy :: [Variable] -> Match -> (Variable, [Variable], [([Pattern],Exp)])
                               -> (Variable, [Variable], Match)
matchfy us o (con,vs,css) = (con,vs,M (vs++us) css o)

-- replace Variables
replaceExp :: Variable -> Variable -> Exp -> Exp
replaceExp v p (Var     s v1)           = Var     s (replaceVar v p v1)
replaceExp v p (Abs     s m b)          = Abs     s m (replaceBind v p b)
replaceExp v p (App     s e1 e2)        = App     s (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (Pair    s e1 e2)        = Pair    s (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (BinLet  s v1 v2 e1 e2) = BinLet  s (replaceVar v p v1) (replaceVar v p v2) (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (Case    s e fm)         = Case    s (replaceExp v p e) (Map.map (substitute v p) fm)
-- replaceExp v p (CaseP   s e fmp)        = CaseP   s (replaceExp v p e) (Map.map ((substitute v p).match) fmp) TODO monading
replaceExp v p (TypeAbs s b)            = TypeAbs s (replaceBind v p b)
replaceExp v p (TypeApp s e t)          = TypeApp s (replaceExp v p e) t
replaceExp v p (Cond    s e1 e2 e3)     = Cond    s (replaceExp v p e1) (replaceExp v p e2) (replaceExp v p e3)
replaceExp v p (UnLet   s v1 e1 e2)     = UnLet   s (replaceVar v p v1) (replaceExp v p e1) (replaceExp v p e2)
replaceExp _ _ e = e

replaceBind :: Variable -> Variable -> Bind a Exp -> Bind a Exp
replaceBind v p (Bind {bSpan=s,var=v1,binder=t,body=exp}) =
  Bind {bSpan=s,var=replaceVar v p v1,binder=t,body=replaceExp v p exp}

replaceVar :: Variable -> Variable -> Variable -> Variable
replaceVar (Variable _ name1) (Variable _ name) v@(Variable span name2)
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
isVar _       = False

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
