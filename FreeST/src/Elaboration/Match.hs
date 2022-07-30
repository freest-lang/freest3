module Elaboration.Match
  ( addMissingVars
  , getConstructors
  , matchFuns
  , isCon
  , isVar
  , pVar 
  , pPats
  , checkNumArgs
  , checkChanVar
  )
where

import           Data.List            (groupBy,sortOn,transpose)
import           Data.Function        ((&))
import           Data.Functor         ((<&>))
import           Control.Monad.Extra  ((&&^))
import           Control.Bool         (ifThenElseM)

import           Syntax.Base
import           Syntax.Expression
import qualified Syntax.Type       as T
import qualified Validation.Rename as R
import           Util.Error
import           Util.FreestState

import qualified Data.Set          as Set
import qualified Data.Map.Strict   as Map

import           Debug.Trace -- debug (used on debugM function)

type Equation = ([Pattern],Exp)

-- Function validation before translation --------------------------

-- check if the number of arguments is the same for every function definition
checkNumArgs :: ParseEnvPat -> FreestState ()
checkNumArgs pep = tMapWithKeyM_ checkNumArgs' pep

checkNumArgs' :: Variable -> [([Pattern],Exp)] -> FreestState ()
checkNumArgs' fn lines  
  | allSame $ map (length.fst) lines = return ()
  | otherwise = addError $ DifNumberOfArguments (getSpan fn) fn 
  where allSame (x:y:ys) = x == y && allSame (y:ys)
        allSame _ = True

checkChanVar :: ParseEnvPat -> FreestState ()
checkChanVar penv = getConstructors >>= -- set with every constructor
  (\cons -> tMapM_ (checkChanVar' cons.transpose.map fst) penv) 

checkChanVar' :: Set.Set Variable -> [[Pattern]] -> FreestState ()
checkChanVar' cons [] = return ()
checkChanVar' cons (xs:xss) 
  | any isVar xs && 
    any isCon xs = do
    let varsF    = map pVar $ filter isVar xs
    let consF    =            filter isCon xs
    let consFVar = Set.fromList $ map pVar consF
    let inter    = Set.intersection cons consFVar
    if Set.null inter then
      -- Channel pattern-matching
      mapM (\v -> addError $ InvalidVariablePatternChan (getSpan v) v) varsF
      -- nested patterns
      >> checkChanVar' cons ( transpose $ map pPats consF )
      -- next columns
      >> checkChanVar' cons xss
    -- Data types pattern-matching
    else return ()
  -- No mixture pattern-matching
  | otherwise = return ()

-- filling functions -----------------------------------------------

addMissingVars :: ParseEnvPat -> ParseEnvPat
addMissingVars pep = Map.map fillVars pep

-- fills every function definition with its own max length of arguments
fillVars :: [Equation] -> [Equation]
fillVars fun = map (fillVars' maxLen) fun
  where maxLen = foldr (max.length.fst) 0 fun

fillVars' :: Int -> Equation -> Equation
fillVars' n (ps,e) = (ps++missingVars,e) 
  where pat = PatVar $ mkVar defaultSpan "_"
        missingVars = replicate (n - (length ps)) pat

-- match -----------------------------------------------------------

matchFuns :: ParseEnvPat -> FreestState ParseEnv
matchFuns pep = mapM matchFun pep

matchFun :: [Equation] -> FreestState ([Variable],Exp)
matchFun xs@((ps,_):_) = mapM newVar ps                       -- creates new vars for the posterior lambda creation
                     >>= \args -> (,) args <$> match args xs 

match :: [Variable] -> [Equation] -> FreestState Exp
match vs x = do
  ifThenElseM (isRuleChan  x)                                 -- observes if it has channel patterns
              (ruleChan vs x) (match' vs x)

match' :: [Variable] -> [Equation] -> FreestState Exp
match' vs x                                                   -- then goes to check other rules
  | isRuleEmpty x = ruleEmpty vs x
  | isRuleVar   x = ruleVar   vs x
  | isRuleCon   x = ruleCon   vs x
  | otherwise     = ruleMix   vs x

-- is rule ---------------------------------------------------------
isRuleEmpty :: [Equation] -> Bool
isRuleEmpty cs = all (null.fst) cs  -- all have to be empty

isRuleVar   :: [Equation] -> Bool
isRuleVar cs = all (check.fst) cs   -- every pattern has to be a var
  where check p = not   (null p)
               && isVar (head p)

isRuleCon   :: [Equation] -> Bool
isRuleCon cs = all (check.fst) cs   -- every pattern has to be a constructor
  where check p = not   (null p)
               && isCon (head p)

isRuleChan  :: [Equation] -> FreestState Bool
isRuleChan cs = b1 &&^ b2           -- it cannot be empty or var, but has to be Con, in adition to all being channels 
  where b1 = return $ (not $ isRuleEmpty cs) 
                   && (not $ isRuleVar cs) 
                   && (isRuleCon cs)
        b2 = and <$> mapM (isChan.head.fst) cs

-- rules -----------------------------------------------------------

-- empty -----------------------------------------------------------
ruleEmpty :: [Variable] -> [Equation] -> FreestState Exp
ruleEmpty _ ((_,e):cs) = (\v -> replaceExp v v e) =<< v
  where v = R.renameVar $ mkVar defaultSpan "_"

-- var -------------------------------------------------------------
ruleVar :: [Variable] -> [Equation] -> FreestState Exp
ruleVar (v:us) cs = match us =<< (mapM replace cs)                                  -- replaces vars for every equation
  where replace (p:ps,e)
          | isPat_ p     = return (ps,e)                                            -- if the variable is '_' there's no need to replace
          | otherwise = (,) ps <$> (replaceExp v (pVar p) e)                        -- otherwise replace the variable in every expression

-- con -------------------------------------------------------------
ruleCon :: [Variable] -> [Equation] -> FreestState Exp
ruleCon (v:us) cs = groupSortBy (pName.head.fst) cs                                 -- group by constructor name
                  & mapM destruct                                                   -- transforms into a case
                >>= mapM (\(con,vs,cs) -> (,) con . (,) vs <$> match (vs++us) cs)   -- matches every case expression, with the case's missing variables
                <&> Case s (Var s v) . Map.fromList                                 -- makes the case
  where s = getSpan v
  
-- rule con aux 
destruct :: [Equation] -> FreestState (Variable, [Variable], [Equation])
destruct l@((p:ps,_):cs) = mapM newVar (pPats p)                                    -- creates new vars, for the case expression and the algorithm
                       <&> flip ((,,) (pVar p)) l'                                  -- transforms into a case
  where l' = map (\(p:ps,e) -> ((pPats p)++ps,e)) l                                 -- unfolds the patterns

-- chan ------------------------------------------------------------
ruleChan :: [Variable] -> [([Pattern],Exp)] -> FreestState Exp
ruleChan (v:us) cs = groupSortBy (pName.head.fst) cs                                      -- group by constructor name
                   & mapM destruct                                                        -- transforms into a case
                 >>= mapM (\(con,vs,cs) -> (,) con . (,) vs <$> match (vs++us) cs)        -- matches every case expression, with the case's missing variables
                 <&> Case s (App s (Var s (mkVar s "collect")) (Var s v)) . Map.fromList  -- makes the case collect
  where s = getSpan v

-- mix -------------------------------------------------------------
ruleMix :: [Variable] -> [Equation] -> FreestState Exp
ruleMix (v:us) cs = do
  cons <- constructors $ getDataType cs -- every [(Constructor,#variables)] of the column type
  groupOn (isVar.head.fst) cs           -- groups in vars and cons, by order
        & mapM (fill v cons)            -- every var is transformed into every constructor
      <&> concat                        -- joins constructors with new constructors, keeping the order
      >>= match (v:us)                  -- matches the result

--rule mix aux
fill :: Variable -> [(Variable,Int)] -> [Equation] -> FreestState [Equation]
fill v cons cs 
  | hasVar cs = fill' v cons cs         -- if there is a var, then all are transformed into every constructor
  | otherwise = return cs               -- if it's a constructor ignore
  where hasVar = isVar.head.fst.head

fill' :: Variable -> [(Variable,Int)] -> [Equation] -> FreestState [Equation]
fill' v _ [] = return []
fill' v cons ((p:ps,e):cs) = (++) <$> mapM (mkCons v' e') cons      -- concats the new constructors with the rest 
                                  <*> fill' v cons cs
  where v' = PatVar $ mkVar (getSpan $ pVar p) "_"                  -- nested Patterns -> Variables
        e' = replaceExp v (pVar p) e                                -- replace the variable that is becoming a Constructor
        mkCons v e (c,n) = (,) (PatCons c (replicate n v):ps) <$> e -- creates a list with the Constuctor and corresponding nested variables
  
-- gets the first contructor name
getDataType :: [([Pattern], Exp)] -> Variable
getDataType cs = filter (isCon.head.fst) cs
               & pVar.head.fst.head

-- returns constructors and the amount of variables they need
constructors :: Variable -> FreestState [(Variable,Int)]
constructors c = (findCons c).(map (snd.snd)).(Map.toList) <$> getTEnv -- finds the constructors from the constructor data type

findCons :: Variable -> [T.Type] -> [(Variable,Int)]
findCons c [] = []
findCons c (t@(T.Almanac _ T.Variant tm):ts) 
  | c `elem` getKeys t = map (\(v,t) -> (v,countArrows t)) (Map.toList tm)  -- if the constructor is inside, this is the correct data type
  | otherwise = findCons c ts                                               -- otherwise search on the next
  where countArrows (T.Arrow _ _ _ t2) = 1 + countArrows t2                 -- the number of arrows gives the number of components of each Constructor
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
-- TODOX check cases
-- replaceExp v p (CasePat s e flp)      = encase =<< (R.renameVar $ mkVar(getSpan e) name)
--   where encase v' = UnLet s v' <$> replaceExp v p e <*> (replaceExp v p =<< match [v'] flp)
--         name = "hidden_var"
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

getConstructors :: FreestState (Set.Set Variable)
getConstructors = Map.elems <$> getTEnv
              <&> map (getKeys.snd)
              <&> concat
              <&> Set.fromList

isChan :: Pattern -> FreestState Bool
isChan (PatVar _)    = return False
isChan (PatCons c _) = getConstructors
                   <&> Set.notMember c

isPat_ :: Pattern -> Bool
isPat_ (PatVar v) = is_ v

newVar :: Pattern -> FreestState Variable
newVar = R.renameVar.pVar

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy apply
  where apply n1 n2 = f n1 == f n2

groupSortBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupOn f . sortOn f
