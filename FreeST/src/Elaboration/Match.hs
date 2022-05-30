module Elaboration.Match
  ( matchFun
    match
  )
where

import           Syntax.Base
import           Syntax.Expression
import           Validation.Rename as R
import qualified Data.Map.Strict   as Map

import           Util.FreestState

--------------- just to remember the format ----------------
-- cases                                                  --
-- type FieldMap  = Map.Map Variable ([Variable], Exp)    --
-- type FieldMapP = Map.Map Variable ([Pattern], Exp)     --
-- functions                                              --
-- type ParseEnv  = Map.Map Variable ([Variable], Exp)    --
-- type ParseEnvP = Map.Map Variable [([Pattern], Exp)]   --
------------------------------------------------------------

--              fun args     patterns, exp   otherwise
data Match = M [Variable] [([Pattern], Exp)] Match
           | ERROR

match :: [([Pattern],Exp)] -> ([Variable],Exp)
match xs@(x:_) = 
  let arguments = map (mkVar) (fst x)  in
  let m         = M arguments xs ERROR in
  let result    = matching m           in
  casefy result
-- TODO
-- criar len [Pattern] variaveis, porque sao os argumentos
-- transformar [([Pattern],Exp)] into Match
-- identificar a rule correta
-- chamar a rule sobre o estado
-- recursivo até chegar à rule empty?

-- > ainda tenho de descobrir como fazer os cases

mkVar :: Pattern -> Variable
mkVar (V var)   = R.renameVar var
mkVar (C var _) = R.renameVar var

-- TODO
casefy :: Matching -> ([Variables],Exp)
casefy (M us cs o) = ([], undefined)

matchFun :: ParseEnvP -> ParseEnv
matchFun pep = Map.map (match) pep

-- TODO
matching :: Match -> Match
matching (M us cs o) 
  | isRuleEmpty cs = ruleEmpty x
  | isRuleVar   cs = matching $ ruleVar x
  | isRuleCon   cs = matching $ ruleCon x
  | otherwise      = matching $ ruleMix x

isRuleEmpty :: Match -> Bool
isRuleEmpty ERROR = False
isRuleEmpty (M us cs o) = and $ map empty.head cs

isRuleVar :: Match -> Bool
isRuleVar ERROR = False
isRuleVar (M _ cs _) = and $ map check.fst.head cs
  where check p = not empty p
               && isVar (head p)

isRuleCon :: Match -> Bool
isRuleCon ERROR = False
isRuleCon (M _ cs _) = and $ map check.fst.head cs
  where check p = not empty p
               && isCon (head p)

isVar :: Pattern -> Bool
isVar (Var _) = True
isVar _       = False

isPat :: Pattern -> Bool
isPat (C _ _) = True
isPat _       = False

ruleEmpty :: Match -> Match
ruleEmpty x = x

ruleVar :: Match -> Match
ruleVar ERROR = ERROR
ruleVar (M us cs o) = matching $ M us' cs' o
  where (v:us') = us
        replace (p:ps,e) = (ps, replaceExp v p e)
        cs' = map replace cs
        
ruleCon :: Match -> Match
ruleCon x = x

ruleMix :: Match -> Match
ruleMix x = x

replaceExp :: Variable -> Variable -> Exp -> Exp
replaceExp v p (Var s v1)               = Var     s (replaceVar v p v1)
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