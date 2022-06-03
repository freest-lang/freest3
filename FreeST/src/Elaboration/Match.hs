module Elaboration.Match
  ( matchFun
 -- , match
  )
where

import           Data.List(groupBy,sortOn)
import           Data.Function((&))

import           Syntax.Base hiding (mkVar)
import           Syntax.Expression
import           Validation.Rename as R
import qualified Data.Map.Strict   as Map

import Data.Traversable
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
matchFun pep = undefined -- mapM (match) pep

match :: [([Pattern],Exp)] -> FreestState ([Variable],Exp)
match xs@(x:_) = do 
  arguments <- mapM mkVar (fst x)  -- TODO make Monad: <-
  let m         = M arguments xs ERROR 
  let result    = matching m           
  return $ casefy result
-- TODO
-- criar len [Pattern] variaveis, porque sao os argumentos
-- transformar [([Pattern],Exp)] into Match
-- identificar a rule correta
-- chamar a rule sobre o estado
-- recursivo até chegar à rule empty?

-- > ainda tenho de descobrir como fazer os cases

matching :: Match -> Match
matching (CaseM v xs) = CaseM v (map (\(a,b,c) -> (a,b,matching c)) xs)
matching x@(M us cs o) 
  | isRuleEmpty x = ruleEmpty x
  | isRuleVar   x = matching $ ruleVar x
  | isRuleCon   x = matching $ ruleCon x
  | otherwise     = matching $ ruleMix x
matching x = x

-- TODO
casefy :: Match -> ([Variable],Exp)
casefy (M us cs o) = ([], undefined)

-- is rule
isRuleEmpty :: Match -> Bool
isRuleEmpty (M us cs o) = and $ map (empty . head) cs
isRuleEmpty _ = False

isRuleVar :: Match -> Bool
isRuleVar (M _ cs _) = and $ map check.fst.head cs
  where check p = not empty p
               && isVar (head p)
isRuleVar _ = False

isRuleCon :: Match -> Bool
isRuleCon (M _ cs _) = and $ map check.fst.head cs
  where check p = not empty p
               && isCon (head p)
isRuleCon _ = False

-- rules
-- TODO
ruleEmpty :: Match -> Match
ruleEmpty (M _ ((_,e):cs) _) = ExpM e

ruleVar :: Match -> Match
ruleVar (M (v:us) cs o) = matching $ M us cs' o
  where cs' = map (\(p:ps,e) -> (ps, replaceExp v p e)) cs

ruleCon :: Match -> Match
ruleCon (M (v:us) cs o) = groupSortBy name.head.fst cs 
                        & map destruct
                        & matchfy us o
                        & \cs' -> CaseM v cs' o

ruleMix :: Match -> Match
ruleMix (M us cs o) = groupOn name.head.fst cs
                    & distribute us o
  where distribute us o [] = o
        distribute us o (cs:css) = M us cs (distribute us o css)

-- rule con aux
destruct :: [([Pattern],Exp)] -> (Variable, [Variable], [([Pattern],Exp)])
destruct l@((p:ps):cs) = (pVar p, newArgs, destruct' l)
  where newArgs = map mkVar (pPats p)

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
replaceExp v p (Abs     s m (Bind t e)) = Abs     s m (Bind t (replaceExp v p e))
replaceExp v p (App     s e1 e2)        = App     s (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (Pair    s e1 e2)        = Pair    s (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (BinLet  s v1 v2 e1 e22) = BinLet  s (replaceVar v p v1) (replaceVar v p v2) (replaceExp v p e1) (replaceExp v p e2)
replaceExp v p (Case    s e fm)         = Case    s (replaceExp v p e) (Map.map (substitute v p) fm)
replaceExp v p (CaseP   s e fmp)        = CaseP   s (replaceExp v p e) (Map.map (substitute v p).match fmp)
replaceExp v p (TypeAbs s (Bind k e))   = TypeAbs s (Bind k (replaceExp v p e))
replaceExp v p (TypeApp s e t)          = TypeApp s (replaceExp v p e) t
replaceExp v p (Cond    s e1 e2 e3)     = Cond    s (replaceExp v p e1) (replaceExp v p e2) (replaceExp v p e3)
replaceExp v p (UnLet   s v1 e1 e2)     = UnLet   s (replaceVar v p v1) (replaceExp v p e1) (replaceExp v p e2)
replaceExp e = e

replaceVar :: Variable -> Variable -> Variable
replaceVar (Variable _ name1) (Variable _ name) v@(Variable span name2)
  | name1 == name2 = Variable span name
  | otherwise      = v

substitute :: Variable -> Variable -> ([Variable],Exp)
substitute v p (vs,e) = (map (replaceVar v p) vs, replaceExp v p e)

-- aux
name :: Pattern -> String
name (V v)   = ""
name (C v _) = intern v

pVar :: Pattern -> Variable
pVar (C v _) = v

pPats :: Pattern -> [Pattern]
pPats (C _ ps) = ps

isVar :: Pattern -> Bool
isVar (Var _) = True
isVar _       = False

isCon :: Pattern -> Bool
isCon (C _ _) = True
isCon _       = False

mkVar :: Pattern -> FreestState Variable -- TODO make Monad
mkVar (V var)   = R.renameVar var
mkVar (C var _) = R.renameVar var

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy apply
  where apply n1 n2 = f n1 == f n2

groupSortBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupOn f . sortOn f
