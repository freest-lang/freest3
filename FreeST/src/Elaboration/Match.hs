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
ruleVar x = x

ruleCon :: Match -> Match
ruleCon x = x

ruleMix :: Match -> Match
ruleMix x = x