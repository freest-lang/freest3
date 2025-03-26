module PatternMatch.Match
  (addMissingVars
  -- , checkChoices
  , checkNumArgs
  , checkChanVar
  , matchFuns
  )
where

import qualified Parse.Phase as PP
import           PatternMatch.Phase
import           Syntax.Base
import           Syntax.Expression
import           Syntax.MkName
import qualified Syntax.Type as T
import           Util.Error
import           Util.State
import qualified Typing.Rename as R

import           Control.Bool (ifThenElseM)
import           Control.Monad (when)
import           Control.Monad.Extra ((&&^))
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.List (groupBy,sortOn,transpose)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Equation = ([Pattern],Exp)

-- Function validation before translation --------------------------

-- -- check if there is choices with the same name as contructors
-- checkChoices :: ParseEnvChoices -> FreestState ()
-- checkChoices pec = do
--   cons <- Set.toList <$> getConstructors -- [Variable]
--   map (\c -> (find (== c) cons,c)) pec
--     & filter (isJust . fst)
--     & mapM_ (\(Just cons,chan) -> addError 
--             $ ConflictChoiceCons (getSpan chan) chan (getSpan cons))

-- check if the number of arguments is the same for every function definition
checkNumArgs :: PP.Defs -> PatternState ()
checkNumArgs = tMapWithKeyM_ checkNumArgs'

checkNumArgs' :: Variable -> [([Pattern],Exp)] -> PatternState ()
checkNumArgs' fn eqs  
  | allSame $ map (length . fst) eqs = return ()                  -- if every line has the same amount of arguments all is fine
  | otherwise = addError $ DifNumberOfArguments (getSpan fn) fn   -- if not there's an error
  where allSame (x:y:ys) = x == y && allSame (y:ys)
        allSame _ = True

-- check if there is a mixture of channel patterns
checkChanVar :: PP.Defs -> PatternState ()
checkChanVar penv = getConstructors >>= -- set with every constructor
  (\cons -> tMapM_ (mapM (checkChanVar' cons) . prep) penv) 
  where prep = transpose . map fst

checkChanVarCase :: FieldList -> PatternState ()
checkChanVarCase fl = getConstructors >>=
  (\cons -> mapM_ (checkChanVar' cons) $ prep fl)
  where prep = transpose . map fst

checkChanVar' :: Set.Set Variable -> [Pattern] -> PatternState ()
checkChanVar' _ [] = return ()
checkChanVar' cons xs
  -- mixture rule
  | any isVar xs && any isCon xs = do                         -- if has at least one var and cons
      let varsF    = map pVar $ filter isVar xs                 -- get vars
      let consF    =            filter isCon xs                 -- get constructors
      let consFVar = Set.fromList $ map pVar consF              -- get constructors' names
      let inter    = Set.intersection cons consFVar             -- intersection between actual constructors and our constructors 
      when (Set.null inter) ( do                                 -- if null they are channels
        -- Channel pattern; error for each variable, since there is channel patterns
        mapM_ (\v -> addError $ InvalidVariablePatternChan (getSpan v) v) varsF
        -- nested patterns: group by name; get nested patterns and transpose them, each column each list ; apply check
        mapM_  (mapM (checkChanVar' cons) . transpose . map pPats) (groupSortBy pName consF)
       )
  | otherwise = return ()   -- every other rule

-- filling functions -----------------------------------------------

addMissingVars :: PP.Defs -> PP.Defs
addMissingVars = Map.map fillVars

-- fills every function definition with its own max length of arguments
fillVars :: [Equation] -> [Equation]
fillVars fun = map (fillVars' maxLen) fun
  where maxLen = foldr (max.length.fst) 0 fun -- finds the maximum number of arguments in a function

fillVars' :: Int -> Equation -> Equation
fillVars' n (ps,e) = (ps++missingVars,e)      -- fills with '_' variables all lines with missing arguments
  where pat = PatVar $ mkWild defaultSpan
        missingVars = replicate (n - length ps) pat

-- match -----------------------------------------------------------

matchFuns :: PP.Defs -> PatternState Defs
matchFuns = mapM matchFun

-- creates new vars for the posterior lambda creation
matchFun :: [Equation] -> PatternState ([Variable],Exp)
matchFun xs@((ps,_):_) = mapM newVar ps >>= \args -> (,) args <$> match args xs 

match :: [Variable] -> [Equation] -> PatternState Exp
match vs x = -- observes if it has channel patterns
  ifThenElseM (isRuleChan  x) (ruleChan vs x) (match' vs x)

match' :: [Variable] -> [Equation] -> PatternState Exp
match' vs x                                                   -- then goes to check other rules
  | isRuleEmpty x = ruleEmpty vs x
  | isRuleVar   x = ruleVar   vs x
  | isRuleCon   x = ruleCon   vs x
  | otherwise     = ruleMix   vs x

-- is rule ---------------------------------------------------------
isRuleEmpty :: [Equation] -> Bool
isRuleEmpty = all (null . fst)  -- all have to be empty

isRuleVar   :: [Equation] -> Bool
isRuleVar = all (check . fst)   -- every pattern has to be a var
  where check p = not (null p) && isVar (head p)

isRuleCon   :: [Equation] -> Bool
isRuleCon = all (check . fst)   -- every pattern has to be a constructor
  where check p = not (null p) && isCon (head p)

isRuleChan  :: [Equation] -> PatternState Bool
isRuleChan cs = b1 &&^ b2           -- it cannot be empty or var, but has to be Con, in adition to all being channels 
  where b1 = return $ not (isRuleEmpty cs)
                   && not (isRuleVar cs) 
                   && isRuleCon cs
        b2 = and <$> mapM (isChan . head . fst) cs

-- rules -----------------------------------------------------------

-- empty -----------------------------------------------------------
ruleEmpty :: [Variable] -> [Equation] -> PatternState Exp
ruleEmpty _ ((_,e):_) = (\v -> replaceExp v v e) =<< v
  where v = R.renameVar $ mkWild defaultSpan

-- var -------------------------------------------------------------
ruleVar :: [Variable] -> [Equation] -> PatternState Exp
ruleVar (v:us) cs = match us =<< mapM replace cs            -- replaces vars for every equation
  where replace (p:ps,e)
          | isPat_ p     = return (ps,e)                    -- if the variable is '_' there's no need to replace
          | otherwise = (,) ps <$> replaceExp v (pVar p) e  -- otherwise replace the variable in every expression

-- con -------------------------------------------------------------
ruleCon :: [Variable] -> [Equation] -> PatternState Exp
ruleCon (v:us) cs = groupSortBy (pName.head.fst) cs                                 -- group by constructor name
                  & mapM destruct                                                   -- transforms into a case
                >>= mapM (\(con,vs,cs) -> (,) con . (,) vs <$> match (vs++us) cs)   -- matches every case expression, with the case's missing variables
                <&> Case s (Var s v) . Map.fromList                                 -- makes the case
  where s = getSpan v
  
-- rule con aux 
destruct :: [Equation] -> PatternState (Variable, [Variable], [Equation])
destruct l@((p:_,_):_) = mapM newVar (pPats p)       -- creates new vars, for the case expression and the algorithm
                       <&> flip ((,,) (pVar p)) l'     -- transforms into a case
  where l' = map (\(p:ps,e) -> (pPats p ++ ps, e)) l   -- unfolds the patterns

-- chan ------------------------------------------------------------
ruleChan :: [Variable] -> [([Pattern],Exp)] -> PatternState Exp
ruleChan (v:us) cs = groupSortBy (pName.head.fst) cs                                -- group by constructor name
                   & mapM destruct                                                  -- transforms into a case
                 >>= mapM (\(con,vs,cs) -> (,) con . (,) vs <$> match (vs++us) cs)  -- matches every case expression, with the case's missing variables
                 <&> Case s (App s (Var s (mkCollect s)) (Var s v)) . Map.fromList  -- makes the case collect
  where s = getSpan v

-- mix -------------------------------------------------------------
ruleMix :: [Variable] -> [Equation] -> PatternState Exp
ruleMix (v:us) cs = do
  cons <- constructors $ getDataType cs -- every [(Constructor,#variables)] of the column type
  -- groups in vars and cons, by order. every var is transformed into every constructor.
  -- joins constructors with new constructors, keeping the order matches the result
  (groupOn (isVar . head . fst) cs & mapM (fill v cons)) >>= match (v : us) . concat


--rule mix aux
fill :: Variable -> [(Variable,Int)] -> [Equation] -> PatternState [Equation]
fill v cons cs 
  | hasVar cs = fill' v cons cs         -- if there is a var, then all are transformed into every constructor
  | otherwise = return cs               -- if it's a constructor ignore
  where hasVar = isVar . head . fst . head

fill' :: Variable -> [(Variable,Int)] -> [Equation] -> PatternState [Equation]
fill' _ _ [] = return []
fill' v cons ((p:ps,e):cs) = (++) <$> mapM (mkCons v' e') cons      -- concats the new constructors with the rest 
                                  <*> fill' v cons cs
  where v' = PatVar $ mkWild (getSpan $ pVar p)                     -- nested Patterns -> Variables
        e' = replaceExp v (pVar p) e                                -- replace the variable that is becoming a Constructor
        mkCons v e (c,n) = (,) (PatCons c (replicate n v):ps) <$> e -- creates a list with the Constuctor and corresponding nested variables
  
-- gets the first contructor name
getDataType :: [([Pattern], Exp)] -> Variable
getDataType cs = filter (isCon . head . fst) cs & pVar . head . fst . head

-- returns constructors and the amount of variables they need
constructors :: Variable -> PatternState [(Variable,Int)]
constructors c = findCons c . map (snd . snd) . Map.toList <$> getTypes  -- finds the constructors from the data type with the constructor c

findCons :: Variable -> [T.Type] -> [(Variable,Int)]
findCons _ [] = []
findCons c (t:ts) 
  | c `elem` getKeys t = consAndNumber t                                -- if the constructor is inside this data type, return its constructors and size
  | otherwise = findCons c ts                                           -- if not continue searching

consAndNumber :: T.Type -> [(Variable,Int)]
consAndNumber (T.Labelled _ T.Variant _ tm) = 
  map (\(v, T.Labelled _ _ _ tm) -> (v, Map.size tm)) (Map.toList tm)

-- retuns the data type constructors
getKeys :: T.Type -> [Variable]
getKeys (T.Labelled _ T.Variant _ tm) = Map.keys tm
getKeys _ = []

-- replace Variables -----------------------------------------------
replaceExp :: Variable -> Variable -> Exp -> PatternState Exp
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
  nVar <- R.renameVar $ Variable (getSpan e) "unLetHiddenVar" (-1)    -- creates an hidden variable
  UnLet s nVar <$> replaceExp v p e
               <*> (replaceExp v p =<< match [nVar] flp)          -- this variables then acts as the pattern variable
replaceExp _ _ e = return e

replaceBind :: Variable -> Variable -> Bind a Exp -> PatternState (Bind a Exp)
replaceBind v p b@(Bind {var=v1,body=exp}) = replaceExp v p exp
                                         <&> (\e -> b {var=replaceVar v p v1,body=e})

replaceVar :: Variable -> Variable -> Variable -> Variable
replaceVar (Variable _ str i) v1 v2
  | intern v1 == intern v2 = Variable (getSpan v2) str i
  | otherwise      = v2

substitute :: Variable -> Variable -> ([Variable],Exp) -> PatternState ([Variable],Exp)
substitute v p (vs,e) = (,) (map (replaceVar v p) vs) <$> replaceExp v p e

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
isCon = not . isVar

-- get every constructor from the file
getConstructors :: PatternState (Set.Set Variable)
getConstructors = Map.elems <$> getTypes
              <&> map (getKeys . snd)
              <&> concat
              <&> Set.fromList

isChan :: Pattern -> PatternState Bool
isChan (PatVar _)    = return False
isChan (PatCons c _) = getConstructors <&> Set.notMember c

isPat_ :: Pattern -> Bool
isPat_ (PatVar v) = isWild v

newVar :: Pattern -> PatternState Variable
newVar = R.renameVar . pVar

-- -- newVar :: Int -> Pattern -> FreestState Variable
-- -- newVar i p = R.renameVar $ updateVar $ pVar p
-- --   where updateVar v = Variable (getSpan v) ("param"++(show i))

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy apply
  where apply n1 n2 = f n1 == f n2

groupSortBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupOn f . sortOn f

-- imapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
-- imapM f l = zipWithM f indexes l
--   where indexes = [0..(length l)]
