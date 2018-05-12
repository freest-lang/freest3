module CodeGen.CodeGen
 ( HaskellCode(..)
 , MonadicMap(..)
 , translate
 , checkMonadicEEnv
 ) where 

import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           Terms.Terms
import  Data.List -- TODO: Delete
  
-- 1st Pass to check which are the monadic functions
type MonadicMap = Map.Map TermVar Bool

-- type ExpEnv = Map.Map TermVar (Params, Expression)
checkMonadicEEnv :: ExpEnv -> MonadicMap
checkMonadicEEnv = Map.foldrWithKey (\f (_, e) acc -> Map.insert f (isMonadic e) acc) Map.empty

isMonadic :: Expression -> Bool  
isMonadic (Pair _ e1 e2) = checkTwoExps e1 e2
isMonadic (UnLet _ _ e1 e2) = checkTwoExps e1 e2
isMonadic (BinLet _ _ _ e1 e2) = checkTwoExps e1 e2
isMonadic (Conditional _ _ e1 e2) = checkTwoExps e1 e2
isMonadic (App _ e1 e2) = checkTwoExps e1 e2
isMonadic (TypeApp _ e _) = isMonadic e
isMonadic (Send _ e1 e2) = True
isMonadic (Receive _ e) =  True
isMonadic (New _ _) =  True
isMonadic (Fork _ e) =  True
isMonadic (Match _ e m) =  True
isMonadic (Case _ e m) = False -- TODO
isMonadic (Select _ l e) =  True
isMonadic _ =  False


checkTwoExps :: Expression -> Expression -> Bool
checkTwoExps e1 e2 =
  let b1 = isMonadic e1
      b2 = isMonadic e2
      in b1 || b2

-- 2nd Pass: Translate from internal representation to Haskell code

type HaskellCode = String
type TranslateMonad = State Int

-- Gets the next fresh var based on the state
nextFresh :: TranslateMonad String
nextFresh = do
  fresh <- get
  modify (+1)
  return $ "_x" ++ show fresh


-- Translate function, takes the monadic map and an expression and returns the translated haskell code

translate :: MonadicMap -> Expression -> TranslateMonad (HaskellCode, Bool)
translate m e = do
  (h1, h2, le, b) <- translate' e
  if b && not (check m le) then -- TODO: map verification
    return $ (h1 ++ "return " ++ h2, b)
--    return $ (h1 ++ " return " ++ h2, b)
  else
    return $ (h1 ++ h2, b)

check :: MonadicMap -> Expression -> Bool
check m (App _ (Variable _ x) _) = Map.member x m && m Map.! x
-- check m (App _ _ (Variable _ x)) = Map.member x m && m Map.! x
check m (Variable _ x) = Map.member x m && m Map.! x
check _ _ = False

-- check :: MonadicMap -> HaskellCode -> Bool
-- check m x
--   | "client1" `isInfixOf` x = Map.member "client1" m && m Map.! "client1"
--   | otherwise = False

translate' :: Expression -> TranslateMonad (HaskellCode, HaskellCode, Expression, Bool)
translate' Unit = return ([], "()", Unit, False)
translate' (Integer i) = return ([], show i, Integer i, False)
translate' (Character c) = return ([], show c, Character c, False)
translate' (Boolean b) =  return ([], show b, Boolean b, False)
translate' (Variable p x) =  return ([], x, Variable p x, False)
translate' (Constructor p c) =  return ([], c, Constructor p c, False)
translate' (Pair _ e1 e2) = do
  (h1, h2, _, _) <- translate' e1
  (h3, h4, le, _) <- translate' e2
  return ([], "(" ++ h1 ++ h2 ++ ", " ++ h3 ++ h4 ++ ")", le, False)

translate' (UnLet _ x e1 e2) = do 
  (h1, h2, _, b1) <- translate' e1
  (h3, h4, le, b2) <- translate' e2
  if b1 then
    return (h1 ++ h2 ++ " >>= \\" ++ x ++ " -> " ++ h3, h4, le, True) 
  else
    return ("let " ++ x ++ " = " ++ h1 ++ h2 ++ " in " ++ h3 ++ h4, [], le, b2)

translate' (BinLet _ x y e1 e2) = do
  (h1, h2, _, b1) <- translate' e1
  (h3, h4, le, b2) <- translate' e2
  if b1 then
    return (h1 ++ h2 ++ " >>= \\(" ++ x ++ "," ++ y ++ ")" ++ " -> " ++ h3, h4, le, True) 
  else
    return ("let (" ++ x ++ "," ++ y ++ ")" ++ " = " ++ h1 ++ h2 ++ " in " ++ h3 ++ h4, [], le, b2)

translate' (Conditional _ c e1 e2) = do
  (c1, c2, _, _) <- translate' c
  (h1, h2, _, b1) <- translate' e1
  (h3, h4, le, b2) <- translate' e2
  return ("if " ++ c1 ++ c2 ++ " then " ++ h1 ++ h2 ++ " else " ++ h3 ++ h4, [] , le, b1 || b2)

translate' (App _ e1 e2) = do
  (h1, h2, le, b1) <- translate' e1
  (h3, h4, _, b2) <- translate' e2
  if b1 then    
    do
      v <- nextFresh
      v <- nextFresh
      v <- nextFresh
      return (h3 ++ h4 ++ " >>= \\ " ++ v ++ " -> " ++ h1, h2, le, b1 || b2)
  else
    return ([] , "(" ++ h1 ++ h2 ++ " " ++ h3 ++ h4 ++ ")", le,  False)

translate' (TypeApp _ e _) = translate' e

translate' (Send p e1 e2) = do
  (h1, h2, _, b1) <- translate' e1
  (h3, h4, _, b2) <- translate' e2
  if b2 then
    do
      v <- nextFresh
      return (h3 ++ h4 ++ " >>= \\" ++ v ++ " -> " ++ "send " ++ h1 ++ h2,
              [], (Variable p "send"), True)
  else
    return ("send " ++ h1 ++ h2 ++ " " ++ h3 ++ h4, [], (Variable p "send"), True)

translate' (Receive p e) = do
  (h1, h2, _, b1) <- translate' e
  if b1 then
    do
      v <- nextFresh
      return ("\\" ++ v ++ " -> " ++ "receive " ++ h1 ++ h2, [], (Variable p "receive"), True)
  else
    return ("receive " ++ h1 ++ h2, [], (Variable p "receive"), True)

translate' (New p _) = return ("new ", [], (Variable p "new"), True)

translate' (Fork p e) = do
  (h1, h2, _, _) <- translate' e
  return ("fork (" ++ h1 ++ h2 ++ ")", [],  (Variable p "Fork"), True)

translate' (Match _ e m) = do
  (h1, h2, le, _) <- translate' e
  (h3, params) <- translateMatchMap m
  v <- nextFresh
  return ("receive " ++ h1 ++ h2 ++ " >>= \\(" ++ v ++  ", " ++ (head params) ++
          ") -> case " ++ v ++ " of " ++ h3, [], le, False)

translate' (Case _ e m) = do
  (h1,h2,le,_) <- translate' e 
  h3 <- translateCaseMap m
  return ("case " ++ h1 ++ h2 ++ " of " ++ h3, [], le, False)

translate' (Select p l e) = do
  (h1, h2, _, _) <- translate' e
  return ("send \"" ++ l ++ "\" " ++ h1 ++ h2, [], (Variable p "select"), True)
  

-- TODO fix params
translateMatchMap :: MatchMap -> TranslateMonad (String, [String])
translateMatchMap = Map.foldlWithKey translateMatchMap' (return ("", []))
  where
    translateMatchMap' acc v (param, e) = do
      (h1, h2, _, _) <- translate' e
      acc' <- acc
      return (fst acc' ++ "\n    \"" ++ v ++ "\" " ++ " -> "
         ++ h1 ++ " return " ++ h2, snd acc' ++ [param])


translateCaseMap :: CaseMap -> TranslateMonad String
translateCaseMap = Map.foldlWithKey translateCaseMap' (return "")
  where
    translateCaseMap' acc v (params, e) = do
      (h1, h2, _, _) <- translate' e
      acc' <- acc
      return (acc' ++ "\n    " ++ v ++ showParams params ++ " -> " ++ h1 ++ h2 ++ " ")

showParams :: Params -> String
showParams as
  | null as = ""
  | otherwise = " " ++ (intercalate " " as)

  


