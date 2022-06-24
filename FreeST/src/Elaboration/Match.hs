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

--                  fun args     patterns, exp   otherwise
data Match = M     [Variable] [([Pattern], Exp)] Match
--                 var        cons      vars      case
           | CaseM Variable [(Variable,[Variable],Match)]
--                 exp
           | ExpM  Exp
           | ERROR 

---------------- TODO for debugging ----------------
instance Show Match where
  show (M us ps o)   = "(m " ++ show us ++ ", " ++ show ps ++ ", " ++ show o ++ ")"
  show (CaseM as xs) = "(casem " ++ show as ++ ", " ++ show xs ++ ")"
  show (ExpM e)      = "expression " ++ show e
  show (ERROR)       = "ERROR"

instance Show Pattern where
  show (V v)    = "Var " ++ show v
  show (C v xs) = "Pat " ++ show v ++ "("++ show xs ++")"
----------------------------------------------------

matchFun :: ParseEnvP -> FreestState ParseEnv
matchFun pep = do
  a <- mapM match pep
  debugM . ("ParseEnv " ++) <$> show =<< (return a)
  return a
-- matchFun pep = sequence $ Map.mapWithKey match pep

match :: [([Pattern],Exp)] -> FreestState ([Variable],Exp)
match xs@(x:_) = do 
  arguments <- mapM newVar (fst x)
  debugM . ("Args " ++) <$> show =<< (return arguments)
  let m = M arguments xs ERROR 
  -- result <- matching m
  result <- matchingPrint m
  return $ casefy arguments result

matchingPrint :: Match -> FreestState Match
matchingPrint x = do
  debugM "# matching"
  debugM . ("Program " ++) <$> show =<< (return x)
  matching x

matching :: Match -> FreestState Match
matching (CaseM v xs) = return.CaseM v =<< (mapM f xs)
  where f (a,b,c) = matchingPrint c >>= return.(,,) a b
-- matching x@(M us cs o) 
--   | isRuleEmpty x = return     $ ruleEmpty x
--   | isRuleVar   x = matching   $ ruleVar x
--   | isRuleCon   x = matching =<< ruleCon x
--   | otherwise     = matching   $ ruleMix x
matching x@(M us cs o) 
  | isRuleEmpty x = ruleEmpty x
  | isRuleVar   x = matchingPrint =<< ruleVar x
  | isRuleCon   x = matchingPrint =<< ruleCon x
  | otherwise     = matchingPrint =<< ruleMix x
matching x = return x

casefy :: [Variable] -> Match -> ([Variable],Exp)
casefy as cs = (as, expfy cs)

expfy :: Match -> Exp
expfy (ExpM e)     = e
expfy (CaseM v cs) = Case s (Var s v) (Map.fromList l)
  where l = map (\(con,vs,mat) -> (con,(vs,expfy mat))) cs
        s = getSpan v
-- expfy ERROR = ? TODO maybe should be added a monad for errors
-- expfy M not expected

-- is rule
isRuleEmpty :: Match -> Bool
isRuleEmpty (M _ cs _) = and $ map (null.fst) cs
isRuleEmpty _ = False

isRuleVar :: Match -> Bool
isRuleVar (M _ cs _) = and $ map (check.fst) cs
  where check p = not   (null p)
               && isVar (head p)
isRuleVar _ = False

isRuleCon :: Match -> Bool
isRuleCon (M _ cs _) = and $ map (check.fst) cs
  where check p = not   (null p)
               && isCon (head p)
isRuleCon _ = False

-- rules
-- ruleEmpty :: Match -> Match
-- ruleEmpty (M _ ((_,e):cs) _) = ExpM e

ruleEmpty :: Match -> FreestState Match
ruleEmpty (M _ ((_,e):cs) _) = do
  debugM "# ruleEmpty"
  return $ ExpM e

-- ruleVar :: Match -> Match
-- ruleVar (M (v:us) cs o) = M us (map replace cs) o
--   where replace (p:ps,e) = (ps, replaceExp v (pVar p) e)

ruleVar :: Match -> FreestState Match
ruleVar (M (v:us) cs o) = do
  debugM "# ruleVar"
  return $ M us (map replace cs) o
  where replace (p:ps,e) = (ps, replaceExp v (pVar p) e)

-- ruleCon :: Match -> FreestState Match
-- ruleCon (M (v:us) cs o) = groupSortBy (name.head.fst) cs
--                         & mapM destruct
--                       >>= mapM (return.matchfy us o) -- ugly
--                       >>= return.CaseM v

ruleCon :: Match -> FreestState Match
ruleCon (M (v:us) cs o) = do
  let a = groupSortBy (name.head.fst) cs
  b <- mapM destruct a
  let c = map (matchfy us o) b
  debugM "# ruleCon"
  return $ CaseM v c

-- ruleMix :: Match -> Match
-- ruleMix (M us cs o) = groupOn (name.head.fst) cs
--                     & distribute us o
--   where distribute us o [] = o
--         distribute us o (cs:css) = M us cs (distribute us o css)

ruleMix :: Match -> FreestState Match
ruleMix (M us cs o) = do
  debugM "# ruleMix"
  groupOn (name.head.fst) cs
                    & distribute us o
                    & fill (head us)
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

-- rule mix aux
fill :: Variable -> Match -> FreestState Match
fill u (M vs cs o)
  | hasVar cs = do
    cs' <- fill' u cs
    o'  <- fill  u o
    return $ M vs cs' o'
  | otherwise = do 
    o' <- fill u o
    return $ M vs cs o'
  where hasVar cs = not   (null cs) 
                 && not   (null (fst (head cs))) 
                 && isVar (head (fst (head cs)))
fill _ x = return x

fill' :: Variable -> [([Pattern],Exp)] -> FreestState [([Pattern],Exp)]
fill' _ [] = return []
fill' v ((p:ps,e):xs) = do
  env <- getTEnv
  t   <- lookup p env
  let fc1 = fillCons v (p:ps) e (getCons t) 
  fc2 <- fill' v xs
  return $ fc1 ++ fc2

fillCons :: Variable -> [Pattern] -> Exp -> [(Variable,Int)] -> [([Pattern],Exp)]
fillCons v (p:ps) e vs = map newP vs
  where newE = replaceExp v p e
        s = getSpan $ pVar p
        p' = C v (replicate n (mkVar s '_'))
        newP (c,n) = (p': ps, newE)

-- returns constructors and the amount of variables they need
getCons :: T.Type -> [(Variable,Int)]
getCons (T.Almanac _ T.Variant tm) = map (\(v,t) -> (v,countArrows t)) (Map.toList tm)
  where countArrows (T.Arrow _ _ _ t2) = 1 + countArrows t2
        countArrows _ = 0 

-- replace Variables
replaceExp :: Variable -> Variable -> Exp -> Exp
replaceExp v p (Var     s v1)          = Var     s (replaceVar v p v1)
replaceExp v p (Abs     s m b)         = Abs     s m (replaceBind v p b)
replaceExp v p (App     s e1 e2)       = App     s (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (Pair    s e1 e2)       = Pair    s (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (BinLet  s v1 v2 e1 e2) = BinLet  s (replaceVar v p v1) (replaceVar v p v2) (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (Case    s e fm)        = Case    s (replaceExp v p e) (Map.map (substitute v p) fm)
-- replaceExp v p (CaseP   s e fmp)       = CaseP   s (replaceExp v p e) (Map.map ((substitute v p).match) fmp) TODO monading
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
