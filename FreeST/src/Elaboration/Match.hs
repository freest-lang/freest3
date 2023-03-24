module Elaboration.Match
  ( addMissingVars
  , checkChoices
  , checkNumArgs
  , checkChanVar
  , matchFuns
  )
where

import           Data.List            (groupBy,sortOn,transpose,find)
import           Data.Function        ((&))
import           Data.Functor         ((<&>))
import           Control.Monad        (zipWithM)
import           Control.Monad.Extra  ((&&^))
import           Control.Bool         (ifThenElseM)
import           Syntax.Base
import           Syntax.MkName
import           Syntax.Expression
import qualified Syntax.Type       as T
import qualified Validation.Rename as R
import           Util.Error
import           Util.FreestState
import           Data.Maybe           (isJust)
import qualified Data.Set          as Set
import qualified Data.Map.Strict   as Map

import           Debug.Trace -- debug (used on debugM function)

type Equation = ([Pattern],Exp)

-- Function validation before translation --------------------------

-- check if there is choices with the same name as contructors
checkChoices :: ParseEnvChoices -> FreestState ()
checkChoices pec = do
  cons <- Set.toList <$> getConstructors -- [Variable]
  map (\c -> (find (== c) cons,c)) pec
    & filter (isJust.fst)
    & mapM_ (\(Just cons,chan) -> addError 
            $ ConflictChoiceCons (getSpan chan) chan (getSpan cons))

-- check if the number of arguments is the same for every function definition
checkNumArgs :: ParseEnvPat -> FreestState ()
checkNumArgs pep = tMapWithKeyM_ checkNumArgs' pep

checkNumArgs' :: Variable -> [([Pattern],Exp)] -> FreestState ()
checkNumArgs' fn lines  
  | allSame $ map (length.fst) lines = return ()                  -- if every line has the same amount of arguments all is fine
  | otherwise = addError $ DifNumberOfArguments (getSpan fn) fn   -- if not there's an error
  where allSame (x:y:ys) = x == y && allSame (y:ys)
        allSame _ = True

-- check if there is a mixture of channel patterns
checkChanVar :: ParseEnvPat -> FreestState ()
checkChanVar penv = getConstructors >>= -- set with every constructor
  (\cons -> tMapM_ ((mapM $ checkChanVar' cons).prep) penv) 
  where prep = transpose.(map fst)

checkChanVarCase :: FieldList -> FreestState ()
checkChanVarCase fl = getConstructors >>=
  (\cons -> mapM_ (checkChanVar' cons) $ prep fl)
  where prep = transpose.(map fst)

checkChanVar' :: Set.Set Variable -> [Pattern] -> FreestState ()
checkChanVar' cons [] = return ()
checkChanVar' cons xs
  -- mixture rule
  | any isVar xs &&                                           -- if has at least one var and cons
    any isCon xs = do
    let varsF    = map pVar $ filter isVar xs                 -- get vars
    let consF    =            filter isCon xs                 -- get constructors
    let consFVar = Set.fromList $ map pVar consF              -- get constructors' names
    let inter    = Set.intersection cons consFVar             -- intersection between actual constructors and our constructors 
    if Set.null inter then do                                 -- if null they are channels
      -- Channel pattern
      mapM_ (\v -> addError                                   -- error for each variable, since there is channel patterns
            $ InvalidVariablePatternChan (getSpan v) v) varsF
      -- nested patterns
      groupSortBy pName consF                                 -- group by name
        & map (transpose.(map pPats))                         -- get nested patterns and transpose them, each column each list
        & mapM_ (mapM $ checkChanVar' cons)                   -- apply check 
    else
      -- Cons pattern
      return ()
  -- every other rule
  | otherwise = return ()

-- filling functions -----------------------------------------------

addMissingVars :: ParseEnvPat -> ParseEnvPat
addMissingVars pep = Map.map fillVars pep

-- fills every function definition with its own max length of arguments
fillVars :: [Equation] -> [Equation]
fillVars fun = map (fillVars' maxLen) fun
  where maxLen = foldr (max.length.fst) 0 fun -- finds the maximum number of arguments in a function

fillVars' :: Int -> Equation -> Equation
fillVars' n (ps,e) = (ps++missingVars,e)      -- fills with '_' variables all lines with missing arguments
  where pat = PatVar $ mkWild defaultSpan
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
  where v = R.renameVar $ mkWild defaultSpan

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
                 <&> Case s (App s (Var s (mkCollect s)) (Var s v)) . Map.fromList  -- makes the case collect
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
  where v' = PatVar $ mkWild (getSpan $ pVar p)                     -- nested Patterns -> Variables
        e' = replaceExp v (pVar p) e                                -- replace the variable that is becoming a Constructor
        mkCons v e (c,n) = (,) (PatCons c (replicate n v):ps) <$> e -- creates a list with the Constuctor and corresponding nested variables
  
-- gets the first contructor name
getDataType :: [([Pattern], Exp)] -> Variable
getDataType cs = filter (isCon.head.fst) cs
               & pVar.head.fst.head

-- returns constructors and the amount of variables they need
constructors :: Variable -> FreestState [(Variable,Int)]
constructors c = (findCons c).(map (snd.snd)).(Map.toList) <$> getTEnv  -- finds the constructors from the data type with the constructor c

findCons :: Variable -> [T.Type] -> [(Variable,Int)]
findCons c [] = []
findCons c (t:ts) 
  | c `elem` getKeys t = consAndNumber t                                -- if the constructor is inside this data type, return its constructors and size
  | otherwise = findCons c ts                                           -- if not continue searching

consAndNumber :: T.Type -> [(Variable,Int)]
consAndNumber (T.Labelled _ T.Variant tm) = 
  map (\(v,(T.Labelled _ _ tm)) -> (v, Map.size tm)) (Map.toList tm)

-- retuns the data type constructors
getKeys :: T.Type -> [Variable]
getKeys (T.Labelled _ T.Variant tm) = Map.keys tm
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
replaceExp v p (UnLet s v1 e1 e2)     = UnLet   s      (replaceVar  v p v1)<$> replaceExp v p e1 <*> replaceExp v p e2
replaceExp v p (CasePat s e flp)      = do
  checkChanVarCase flp                                            -- checks if there are variables with channel patterns
  nVar <- R.renameVar $ Variable (getSpan e) "unLetHiddenVar"     -- creates an hidden variable
  UnLet s nVar <$> replaceExp v p e
               <*> (replaceExp v p =<< match [nVar] flp)          -- this variables then acts as the pattern variable
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

-- get every constructor from the file
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
isPat_ (PatVar v) = isWild v

newVar :: Pattern -> FreestState Variable
newVar = R.renameVar.pVar

-- newVar :: Int -> Pattern -> FreestState Variable
-- newVar i p = R.renameVar $ updateVar $ pVar p
--   where updateVar v = Variable (getSpan v) ("param"++(show i))

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy apply
  where apply n1 n2 = f n1 == f n2

groupSortBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupOn f . sortOn f

-- imapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
-- imapM f l = zipWithM f indexes l
--   where indexes = [0..(length l)]
