module CodeGen.Annotation where

import           Syntax.Expressions
import           Syntax.Base -- Test
import           Syntax.Types -- Test
import           Syntax.Show -- Test ?
import           Syntax.Kinds -- Test
import           Syntax.Schemes
import           Syntax.ProgramVariables
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map

import qualified Data.Traversable as Traversable

import Utils.PreludeLoader
-- type AST = Map.Map Expression Bool

-- IOType
data NodeState =
    IOState
  | PureState
  | ArrowState NodeState NodeState
  deriving (Eq, Show) -- Show debug

type AVarEnv = Map.Map ProgVar NodeState -- TODO: change name

instance Ord NodeState where
  IOState   <= (ArrowState _ _) = True  
  PureState <= IOState          = True  
  _         <= _                = False

-- type VarEnv = Map.Map ProgVar TypeScheme
-- type TypeEnv = Map.Map TypeVar (Kind, TypeScheme)

annotateVenv :: TypeEnv -> VarEnv -> AVarEnv
annotateVenv tenv =
  Map.foldrWithKey (\k t avenv -> Map.insert k (typeAnnotation tenv t) avenv) Map.empty

 
-- annotates each type with information about it's state (NodeState)
typeAnnotation :: TypeEnv -> TypeScheme -> NodeState
typeAnnotation _ (TypeScheme _ _ (Basic _ _)) = PureState
typeAnnotation _ (TypeScheme _ _ (TypeVar _ _)) = PureState -- ??
typeAnnotation tenv (TypeScheme _ _ (Fun _ _ t1 t2)) =
  ArrowState (typeAnnotation tenv (fromType t1)) (typeAnnotation tenv (fromType t2))
typeAnnotation tenv (TypeScheme _ _ (PairType _ t1 t2)) =
  max (typeAnnotation tenv (fromType t1)) (typeAnnotation tenv (fromType t2))
typeAnnotation _ (TypeScheme _ _ (Datatype _ _)) = PureState -- TODO
typeAnnotation tenv (TypeScheme _ _ (TypeName _ x)) =
  typeAnnotation tenv $ snd $ tenv Map.! x
typeAnnotation _ _ = IOState

-- TODO: CHANGE NAME
top :: ExpEnv -> TypeEnv -> VarEnv -> AVarEnv
top eenv tenv venv = findFixedPoint eenv (annotateVenv tenv venv)

findFixedPoint :: ExpEnv -> AVarEnv -> AVarEnv
findFixedPoint eenv avenv
  | avenv == avenv' = avenv
  | otherwise = findFixedPoint eenv avenv'
  where avenv' = annotateFunction avenv eenv

annotateFunction :: AVarEnv -> ExpEnv -> AVarEnv
annotateFunction = Map.foldrWithKey (\f e m -> insert f e m)
  where insert f e m = Map.adjust (updateRetType (annFun m e)) f m
          
updateRetType :: NodeState -> NodeState -> NodeState
updateRetType upState (ArrowState s1 s2)  =
  ArrowState s1 $ updateRetType upState s2
updateRetType upState _ = upState

toList :: NodeState -> [NodeState]
toList (ArrowState s1 s2) = s1 : toList s2
toList s                  = [s]

annFun :: AVarEnv -> Expression -> NodeState
annFun _ (Unit _) = PureState
annFun _ (Integer _ _) = PureState
annFun _ (Character _ _) = PureState
annFun _ (Boolean _ _) = PureState
annFun m (ProgVar _ x) = annVar m x
annFun m (Lambda _ _ _ _ e) = annFun m e
annFun m (App _ e1 e2) = max (annFun m e1) (annFun m e2)
annFun m (Pair _ e1 e2) = max (annFun m e1) (annFun m e2)
annFun m (BinLet _ _ _ e1 e2) = max (annFun m e1) (annFun m e2)
annFun m (Case _ e cm) = PureState -- TODO: TMP
annFun m (TypeApp _ x _) = annVar m x
annFun m (Conditional _ e1 e2 e3) = max (max (annFun m e1) (annFun m e2)) (annFun m e3)
annFun m (UnLet _ _ e1 e2) = max (annFun m e1) (annFun m e2)
-- Session types & Fork
annFun _ _ = IOState

annVar :: AVarEnv -> ProgVar -> NodeState
annVar m x
  | Map.member x m = last $ toList $ m Map.! x
  | otherwise      = PureState

type HaskellCode = String

type TranslateMonad = State Int

-- Gets the next fresh var based on the state
nextFresh :: TranslateMonad String
nextFresh = do
  fresh <- get
  modify (+1)
  return $ "_x" ++ show fresh

-- translateEEnv :: ExpEnv -> TypeEnv -> VarEnv -> HaskellCode
-- translateEEnv eenv tenv venv =
--   let m = top eenv tenv venv in
--     Map.foldrWithKey (\f e acc -> acc ++ "\n\n" ++ show f ++ " = "
--                        ++ (fst $ evalState(translate m (isIO (m Map.! f)) e) 0)) "" eenv


-- --translateExpr :: Code to translate -> Expected -> Found -> Translated code
-- translateExpr :: HaskellCode -> Bool -> Bool -> HaskellCode -- TODO: check other cases
-- translateExpr code True False = "return1 " ++ code
-- translateExpr code _ _ = code

-- translate :: AVarEnv -> Bool -> Expression -> TranslateMonad (HaskellCode, Bool)
-- translate _ b (Unit _) = return $ (translateExpr "()" b False, b)
-- translate _ b (Integer _ i) = return $ (translateExpr (show i) b False, b)
-- translate _ b (Boolean _ b1) = return $ (translateExpr (show b1) b False, b)
-- translate avenv b (ProgVar _ x) =
-- --  error $ show avenv
--   case avenv Map.!? x of
--     Just s -> do
--       let io = isIO s
--       return $ (translateExpr (show x) io b, io)
--     Nothing ->
--       if b
--       then return $ ("return2 " ++ show x, True) -- TODO: ??
--       else return $ (translateExpr (show x) False False, False) -- TODO: False False ?? 

-- translate avenv b (Lambda _ m x t e) = do -- PARAMS HERE
--   -- TODO: typeEnv -> Map.empty
--   -- TODO: should not run typeAnnotation again
--   h1 <- translate (Map.insert x (typeAnnotation Map.empty (fromType t)) avenv) b e
-- --  let h1 = translate avenv b e in
--   return $ ("\\" ++ show x ++ showArrow m ++ (fst h1), b)
  
--   -- TODO: pensar args e lets (intro de vars)
-- translate avenv b (UnLet _ x e1 e2) = do
-- --  let avenv1 = (Map.insert x PureState avenv) -- TODO: Pure?
--   (h1, b1) <- translate avenv b e1
--   (h2, b2) <- translate avenv b e2
--   -- TODO: Are there other cases??  
--   if b then
--     return $ (h1 ++ " >>= \\" ++ show x ++ " -> " ++ h2, b) -- TODO: b1 || b2 ??
--   else
--     return $ ("let " ++ show x ++ " = " ++ h1 ++ " in " ++ h2, False)

-- translate avenv b (Conditional _ e1 e2 e3) = do -- TODO: if b1, tirar do monad
--   (h1, b1) <- translate avenv False e1
--   (h2, b2) <- translate avenv b e2
--   (h3, b3) <- translate avenv b e3
--   return ("if " ++ h1 ++ " then " ++ h2 ++ " else " ++ h3, b1 || b2 || b3)
  
-- translate avenv b (BinLet _ x y e1 e2) = do

--   -- let avenv1 = (Map.insert x PureState avenv)
--   --     avenv2 = (Map.insert y PureState avenv1)
--   (h1, b1) <- translate avenv b e1
--   (h2, b2) <-  translate avenv b e2
--   -- TODO: Are there other cases??  
--   if b then
--     return $ (h1 ++ " >>= \\(" ++ show x ++ "," ++ show y ++ ") -> " ++ h2, b) -- TODO: b1 || b2 ??
--   else
--     return $ ("let (" ++ show x ++ "," ++ show y ++ ") = " ++ h1 ++ " in " ++ h2, False)
        
-- translate avenv _ (Fork _ e) = do
--   h <- translate avenv True e
--   return $ ("_fork (" ++ (fst h) ++ ")", True) -- " >> return ())"

-- translate _ _ (New _ _) = return $ ("_new", True)

-- translate avenv _ (Receive _ e) = do -- TODO: not ok, argument cant be IO
--   (h, b) <- translate avenv False e  
--   if b then do
--     v <- nextFresh
--     return (h ++ " >>= \\" ++ v ++ " -> " ++ "_receive " ++ v, True)
--   else
--     return ("_receive " ++ h, True)

-- translate avenv _ (Send _ e1) = do -- ver arg
--   (h1, _) <- translate avenv False e1
--   return ("(_send " ++ h1 ++ ")", True)

-- translate avenv b e@(App _ e1 e2) = do
--   (h1, b1) <- translate avenv b e1 -- aqui nao pode ser b/False, tem de ser o param...
--   (h2, _)  <- translate avenv False e2 -- aqui nao pode ser b/False, tem de ser o param...
--   if b && not b1 then
--     return $ ("return (" ++ h1 ++ " " ++ h2 ++ ")", True)
--   else
--     return $ ("(" ++ h1 ++ " " ++ h2 ++ ")", b)

-- translate _ _ e = return $ ("ERR: |" ++ show e ++ "|", False)



-- sender s 10
-- App (App sender s) 10

isIO :: NodeState -> Bool
isIO (ArrowState _ s2) = isIO s2
isIO IOState           = True
isIO _                 = False






-- TESTING

  
-- Simple mutually recursive test
mutRecVenv :: VarEnv
mutRecVenv = Map.union prelude (Map.fromList [(mkVar (Pos 21 1) "a",TypeScheme (Pos 21 5) [] (Basic (Pos 21 5) IntType)),(mkVar (Pos 17 1) "g",TypeScheme (Pos 17 5) [] (Basic (Pos 17 5) IntType)),(mkVar (Pos 25 1) "main",TypeScheme (Pos 25 8) [] (Basic (Pos 25 8) IntType))])
mutRecEenv :: ExpEnv
mutRecEenv = Map.fromList [(mkVar (Pos 21 1) "a",UnLet (Pos 23 3) (mkVar (Pos 23 7) "0_x") (ProgVar (Pos 23 11) (mkVar (Pos 23 11) "g")) (ProgVar (Pos 23 16) (mkVar (Pos 23 16) "0_x"))),(mkVar (Pos 17 1) "g",UnLet (Pos 19 3) (mkVar (Pos 19 7) "1__") (Fork (Pos 19 11) (Unit (Pos 19 16))) (ProgVar (Pos 19 22) (mkVar (Pos 19 22) "a"))),(mkVar (Pos 25 1) "main",ProgVar (Pos 26 8) (mkVar (Pos 26 8) "g"))]


-- is double program

isDoubleVenv :: VarEnv
isDoubleVenv = Map.union prelude (Map.fromList [(mkVar (Pos 10 1) "f",TypeScheme (Pos 10 10) [] (Fun (Pos 10 10) Un (Message (Pos 10 5) In IntType) (Basic (Pos 10 13) IntType))),(mkVar (Pos 1 1) "main", TypeScheme (Pos 1 8) [] (Basic (Pos 1 8) BoolType)),(mkVar (Pos 13 1) "sender", TypeScheme (Pos 13 15) [] (Fun (Pos 13 15) Un (Message (Pos 13 10) Out IntType) (Fun (Pos 13 22) Un (Basic (Pos 13 18) IntType) (Skip (Pos 13 25)))))])

isDoubleEenv :: ExpEnv
isDoubleEenv = Map.fromList [(mkVar (Pos 10 1) "f",Lambda (Pos 11 3) Un (mkVar (Pos 11 3) "0_c") (Message (Pos 10 5) In IntType) (BinLet (Pos 11 7) (mkVar (Pos 11 11) "1_x") (mkVar (Pos 11 14) "2_c") (Receive (Pos 11 18) (ProgVar (Pos 11 26) (mkVar (Pos 11 26) "0_c"))) (ProgVar (Pos 11 31) (mkVar (Pos 11 31) "1_x")))),(mkVar (Pos 1 1) "main",BinLet (Pos 3 3) (mkVar (Pos 3 7) "3_s") (mkVar (Pos 3 10) "4_r") (New (Pos 3 14) (Message (Pos 3 18) Out IntType)) (UnLet (Pos 4 3) (mkVar (Pos 4 7) "5__") (Fork (Pos 4 11) (App (Pos 4 17) (App (Pos 4 17) (ProgVar (Pos 4 17) (mkVar (Pos 4 17) "sender")) (ProgVar (Pos 4 24) (mkVar (Pos 4 24) "3_s"))) (Integer (Pos 4 26) 10))) (Conditional (Pos 5 3) (App (Pos 5 7) (App (Pos 5 7) (ProgVar (Pos 5 20) (mkVar (Pos 5 20) "(==)")) (App (Pos 5 7) (App (Pos 5 7) (ProgVar (Pos 5 7) (mkVar (Pos 5 7) "div")) (App (Pos 5 12) (ProgVar (Pos 5 12) (mkVar (Pos 5 12) "f")) (ProgVar (Pos 5 14) (mkVar (Pos 5 14) "4_r")))) (Integer (Pos 5 17) 2))) (Integer (Pos 5 23) 5)) (Boolean (Pos 6 5) True) (Boolean (Pos 8 5) False)))),(mkVar (Pos 13 1) "sender",Lambda (Pos 14 8) Un (mkVar (Pos 14 8) "6_c") (Message (Pos 13 10) Out IntType) (Lambda (Pos 14 10) Un (mkVar (Pos 14 10) "7_i") (Basic (Pos 13 18) IntType) (App (Pos 14 14) (Send (Pos 14 14) (ProgVar (Pos 14 19) (mkVar (Pos 14 19) "6_c"))) (App (Pos 14 22) (App (Pos 14 22) (ProgVar (Pos 14 24) (mkVar (Pos 14 24) "(*)")) (ProgVar (Pos 14 22) (mkVar (Pos 14 22) "7_i"))) (Integer (Pos 14 26) 2)))))]



-- -- venv tests
-- anbnVenv :: VarEnv
-- anbnVenv = Map.fromList [(mkVar (Pos 16 1) "client",TypeScheme (Pos 16 14) [] (Fun (Pos 16 14) Un (Basic (Pos 16 10) IntType) (Fun (Pos 16 62) Un (Choice (Pos 16 17) Out (Map.fromList [(mkVar (Pos 16 19) "A",Rec (Pos 16 22) (TypeVarBind (Pos 16 26) (mkVar (Pos 16 26) "0_x") (Kind (Pos 16 28) Session Lin)) (Choice (Pos 16 32) Out (Map.fromList [(mkVar (Pos 16 34) "A",Semi (Pos 16 38) (TypeVar (Pos 16 37) (mkVar (Pos 16 37) "0_x")) (Choice (Pos 16 40) Out (Map.fromList [(mkVar (Pos 16 42) "B",Skip (Pos 16 45))]))),(mkVar (Pos 16 52) "B",Skip (Pos 16 55))])))])) (Skip (Pos 16 65))))),(mkVar (Pos 22 1) "client'",TypeScheme (Pos 22 11) [TypeVarBind (Pos 22 18) (mkVar (Pos 22 18) "5_\945") (Kind (Pos 22 22) Session Lin)] (Fun (Pos 22 32) Un (Basic (Pos 22 28) IntType) (Fun (Pos 22 79) Un (Semi (Pos 22 75) (Rec (Pos 22 36) (TypeVarBind (Pos 22 40) (mkVar (Pos 22 40) "6_x") (Kind (Pos 22 42) Session Lin)) (Choice (Pos 22 46) Out (Map.fromList [(mkVar (Pos 22 48) "A",Semi (Pos 22 52) (TypeVar (Pos 22 51) (mkVar (Pos 22 51) "6_x")) (Choice (Pos 22 54) Out (Map.fromList [(mkVar (Pos 22 56) "B",Skip (Pos 22 59))]))),(mkVar (Pos 22 66) "B",Skip (Pos 22 69))]))) (TypeVar (Pos 22 77) (mkVar (Pos 22 77) "5_\945"))) (TypeVar (Pos 22 82) (mkVar (Pos 22 82) "5_\945"))))),(mkVar (Pos 51 1) "main",TypeScheme (Pos 51 8) [] (Basic (Pos 51 8) UnitType)),(mkVar (Pos 32 1) "server",TypeScheme (Pos 32 55) [] (Fun (Pos 32 55) Un (Choice (Pos 32 10) In (Map.fromList [(mkVar (Pos 32 12) "A",Rec (Pos 32 15) (TypeVarBind (Pos 32 19) (mkVar (Pos 32 19) "17_x") (Kind (Pos 32 21) Session Lin)) (Choice (Pos 32 25) In (Map.fromList [(mkVar (Pos 32 27) "A",Semi (Pos 32 31) (TypeVar (Pos 32 30) (mkVar (Pos 32 30) "17_x")) (Choice (Pos 32 33) In (Map.fromList [(mkVar (Pos 32 35) "B",Skip (Pos 32 38))]))),(mkVar (Pos 32 45) "B",Skip (Pos 32 48))])))])) (Skip (Pos 32 58)))),(mkVar (Pos 39 1) "server'",TypeScheme (Pos 39 11) [TypeVarBind (Pos 39 18) (mkVar (Pos 39 18) "21_\945") (Kind (Pos 39 22) Session Lin)] (Fun (Pos 39 72) Un (Semi (Pos 39 68) (Rec (Pos 39 29) (TypeVarBind (Pos 39 33) (mkVar (Pos 39 33) "22_x") (Kind (Pos 39 35) Session Lin)) (Choice (Pos 39 39) In (Map.fromList [(mkVar (Pos 39 41) "A",Semi (Pos 39 45) (TypeVar (Pos 39 44) (mkVar (Pos 39 44) "22_x")) (Choice (Pos 39 47) In (Map.fromList [(mkVar (Pos 39 49) "B",Skip (Pos 39 52))]))),(mkVar (Pos 39 59) "B",Skip (Pos 39 62))]))) (TypeVar (Pos 39 70) (mkVar (Pos 39 70) "21_\945"))) (TypeVar (Pos 39 75) (mkVar (Pos 39 75) "21_\945"))))]


-- idProg :: ExpEnv
-- idProg = Map.fromList [(mkVar (Pos 1 1) "id", Lambda (Pos 2 4) Un (mkVar (Pos 2 4) "1_x") (TypeVar (Pos 1 23) (mkVar (Pos 1 23) "0_a")) (ProgVar (Pos 2 8) (mkVar (Pos 2 8) "1_x"))), (mkVar (Pos 4 1) "main",App (Pos 5 8) (TypeApp (Pos 5 8) (mkVar (Pos 5 8) "id") [Basic (Pos 5 11) IntType]) (Integer (Pos 5 16) 5))]

-- sendRcvProg :: ExpEnv
-- sendRcvProg = Map.fromList [(mkVar (Pos 11 1) "client",Lambda (Pos 12 8) Un (mkVar (Pos 12 8) "0_c") (Semi (Pos 11 14) (Message (Pos 11 10) Out IntType) (Semi (Pos 11 20) (Message (Pos 11 15) In BoolType) (Skip (Pos 11 21)))) (UnLet (Pos 13 3) (mkVar (Pos 13 7) "1_c1") (App (Pos 13 12) (Send (Pos 13 12) (ProgVar (Pos 13 17) (mkVar (Pos 13 17) "0_c"))) (Integer (Pos 13 19) 5)) (BinLet (Pos 14 3) (mkVar (Pos 14 7) "2_b") (mkVar (Pos 14 10) "3_c2") (Receive (Pos 14 15) (ProgVar (Pos 14 23) (mkVar (Pos 14 23) "1_c1"))) (Unit (Pos 15 6))))),(mkVar (Pos 1 1) "main",BinLet (Pos 3 3) (mkVar (Pos 3 7) "4_w") (mkVar (Pos 3 10) "5_r") (New (Pos 3 14) (Semi (Pos 3 22) (Message (Pos 3 18) Out IntType) (Semi (Pos 3 28) (Message (Pos 3 23) In BoolType) (Skip (Pos 3 29))))) (UnLet (Pos 4 3) (mkVar (Pos 4 7) "6_x") (Fork (Pos 4 11) (App (Pos 4 17) (ProgVar (Pos 4 17) (mkVar (Pos 4 17) "client")) (ProgVar (Pos 4 24) (mkVar (Pos 4 24) "4_w")))) (BinLet (Pos 5 3) (mkVar (Pos 5 7) "7_n") (mkVar (Pos 5 10) "8_r1") (Receive (Pos 5 15) (ProgVar (Pos 5 23) (mkVar (Pos 5 23) "5_r"))) (UnLet (Pos 6 3) (mkVar (Pos 6 7) "9_r2") (App (Pos 6 12) (Send (Pos 6 12) (ProgVar (Pos 6 17) (mkVar (Pos 6 17) "8_r1"))) (App (Pos 6 21) (App (Pos 6 21) (ProgVar (Pos 6 23) (mkVar (Pos 6 23) "(>=)")) (ProgVar (Pos 6 21) (mkVar (Pos 6 21) "7_n"))) (Integer (Pos 6 26) 0))) (Unit (Pos 7 3))))))]

-- intListSizeProg :: ExpEnv
-- intListSizeProg = Map.fromList [(mkVar (Pos 3 1) "length'",Lambda (Pos 4 9) Un (mkVar (Pos 4 9) "0_l") (TypeName (Pos 3 11) (mkVar (Pos 3 11) "IntList")) (Case (Pos 5 3) (ProgVar (Pos 5 8) (mkVar (Pos 5 8) "0_l")) (Map.fromList [(mkVar (Pos 7 5) "Cons",([mkVar (Pos 7 10) "1_x",mkVar (Pos 7 12) "2_y"],App (Pos 7 17) (App (Pos 7 17) (ProgVar (Pos 7 19) (mkVar (Pos 7 19) "(+)")) (Integer (Pos 7 17) 1)) (App (Pos 7 21) (ProgVar (Pos 7 21) (mkVar (Pos 7 21) "length'")) (ProgVar (Pos 7 29) (mkVar (Pos 7 29) "2_y"))))),(mkVar (Pos 6 5) "Nil",([],Integer (Pos 6 12) 0))]))),(mkVar (Pos 10 1) "main",App (Pos 11 8) (ProgVar (Pos 11 8) (mkVar (Pos 11 8) "length'")) (App (Pos 11 17) (App (Pos 11 17) (ProgVar (Pos 11 17) (mkVar (Pos 11 17) "Cons")) (Integer (Pos 11 22) 5)) (App (Pos 11 25) (App (Pos 11 25) (ProgVar (Pos 11 25) (mkVar (Pos 11 25) "Cons")) (Integer (Pos 11 30) 7)) (App (Pos 11 33) (App (Pos 11 33) (ProgVar (Pos 11 33) (mkVar (Pos 11 33) "Cons")) (Integer (Pos 11 38) 23)) (App (Pos 11 42) (App (Pos 11 42) (ProgVar (Pos 11 42) (mkVar (Pos 11 42) "Cons")) (Integer (Pos 11 47) 4)) (ProgVar (Pos 11 49) (mkVar (Pos 11 49) "Nil")))))))]


-- anbnProg :: ExpEnv
-- anbnProg = Map.fromList [(mkVar (Pos 16 1) "client",Lambda (Pos 17 8) Un (mkVar (Pos 17 8) "1_n") (Basic (Pos 16 10) IntType) (Lambda (Pos 17 10) Un (mkVar (Pos 17 10) "2_c") (Choice (Pos 16 17) Out (Map.fromList [(mkVar (Pos 16 19) "A",Rec (Pos 16 22) (TypeVarBind (Pos 16 26) (mkVar (Pos 16 26) "3_x") (Kind (Pos 16 28) Session Lin)) (Choice (Pos 16 32) Out (Map.fromList [(mkVar (Pos 16 34) "A",Semi (Pos 16 38) (TypeVar (Pos 16 37) (mkVar (Pos 16 37) "3_x")) (Choice (Pos 16 40) Out (Map.fromList [(mkVar (Pos 16 42) "B",Skip (Pos 16 45))]))),(mkVar (Pos 16 52) "B",Skip (Pos 16 55))])))])) (UnLet (Pos 18 3) (mkVar (Pos 18 7) "4_c") (Select (Pos 18 11) (mkVar (Pos 18 18) "A") (ProgVar (Pos 18 20) (mkVar (Pos 18 20) "2_c"))) (App (Pos 19 3) (App (Pos 19 3) (TypeApp (Pos 19 3) (mkVar (Pos 19 3) "client'") [Skip (Pos 19 11)]) (App (Pos 19 18) (App (Pos 19 18) (ProgVar (Pos 19 20) (mkVar (Pos 19 20) "(-)")) (ProgVar (Pos 19 18) (mkVar (Pos 19 18) "1_n"))) (Integer (Pos 19 22) 1))) (ProgVar (Pos 19 25) (mkVar (Pos 19 25) "4_c")))))),(mkVar (Pos 22 1) "client'",Lambda (Pos 23 9) Un (mkVar (Pos 23 9) "7_n") (Basic (Pos 22 28) IntType) (Lambda (Pos 23 11) Un (mkVar (Pos 23 11) "8_c") (Semi (Pos 22 75) (Rec (Pos 22 36) (TypeVarBind (Pos 22 40) (mkVar (Pos 22 40) "9_x") (Kind (Pos 22 42) Session Lin)) (Choice (Pos 22 46) Out (Map.fromList [(mkVar (Pos 22 48) "A",Semi (Pos 22 52) (TypeVar (Pos 22 51) (mkVar (Pos 22 51) "9_x")) (Choice (Pos 22 54) Out (Map.fromList [(mkVar (Pos 22 56) "B",Skip (Pos 22 59))]))),(mkVar (Pos 22 66) "B",Skip (Pos 22 69))]))) (TypeVar (Pos 22 77) (mkVar (Pos 22 77) "5_\945"))) (Conditional (Pos 24 3) (App (Pos 24 6) (App (Pos 24 6) (ProgVar (Pos 24 8) (mkVar (Pos 24 8) "(==)")) (ProgVar (Pos 24 6) (mkVar (Pos 24 6) "7_n"))) (Integer (Pos 24 11) 0)) (Select (Pos 26 5) (mkVar (Pos 26 12) "B") (ProgVar (Pos 26 14) (mkVar (Pos 26 14) "8_c"))) (UnLet (Pos 28 5) (mkVar (Pos 28 9) "10_c") (Select (Pos 28 13) (mkVar (Pos 28 20) "A") (ProgVar (Pos 28 22) (mkVar (Pos 28 22) "8_c"))) (UnLet (Pos 29 5) (mkVar (Pos 29 9) "11_c") (App (Pos 29 13) (App (Pos 29 13) (TypeApp (Pos 29 13) (mkVar (Pos 29 13) "client'") [Semi (Pos 29 31) (Choice (Pos 29 21) Out (Map.fromList [(mkVar (Pos 29 23) "B",Skip (Pos 29 26))])) (TypeVar (Pos 29 33) (mkVar (Pos 29 33) "5_\945"))]) (App (Pos 29 37) (App (Pos 29 37) (ProgVar (Pos 29 39) (mkVar (Pos 29 39) "(-)")) (ProgVar (Pos 29 37) (mkVar (Pos 29 37) "7_n"))) (Integer (Pos 29 41) 1))) (ProgVar (Pos 29 44) (mkVar (Pos 29 44) "10_c"))) (Select (Pos 30 5) (mkVar (Pos 30 12) "B") (ProgVar (Pos 30 14) (mkVar (Pos 30 14) "11_c")))))))),(mkVar (Pos 51 1) "main",BinLet (Pos 53 3) (mkVar (Pos 53 7) "12_w") (mkVar (Pos 53 10) "13_r") (New (Pos 53 14) (Choice (Pos 53 18) Out (Map.fromList [(mkVar (Pos 53 20) "A",Rec (Pos 53 23) (TypeVarBind (Pos 53 27) (mkVar (Pos 53 27) "14_x") (Kind (Pos 53 29) Session Lin)) (Choice (Pos 53 33) Out (Map.fromList [(mkVar (Pos 53 35) "A",Semi (Pos 53 39) (TypeVar (Pos 53 38) (mkVar (Pos 53 38) "14_x")) (Choice (Pos 53 41) Out (Map.fromList [(mkVar (Pos 53 43) "B",Skip (Pos 53 46))]))),(mkVar (Pos 53 53) "B",Skip (Pos 53 56))])))]))) (UnLet (Pos 54 3) (mkVar (Pos 54 7) "15_t") (Fork (Pos 54 11) (App (Pos 54 17) (App (Pos 54 17) (ProgVar (Pos 54 17) (mkVar (Pos 54 17) "client")) (Integer (Pos 54 24) 25)) (ProgVar (Pos 54 27) (mkVar (Pos 54 27) "12_w")))) (UnLet (Pos 55 3) (mkVar (Pos 55 7) "16_r") (App (Pos 55 11) (ProgVar (Pos 55 11) (mkVar (Pos 55 11) "server")) (ProgVar (Pos 55 18) (mkVar (Pos 55 18) "13_r"))) (Unit (Pos 56 3))))),(mkVar (Pos 32 1) "server",Lambda (Pos 33 8) Un (mkVar (Pos 33 8) "18_c") (Choice (Pos 32 10) In (Map.fromList [(mkVar (Pos 32 12) "A",Rec (Pos 32 15) (TypeVarBind (Pos 32 19) (mkVar (Pos 32 19) "19_x") (Kind (Pos 32 21) Session Lin)) (Choice (Pos 32 25) In (Map.fromList [(mkVar (Pos 32 27) "A",Semi (Pos 32 31) (TypeVar (Pos 32 30) (mkVar (Pos 32 30) "19_x")) (Choice (Pos 32 33) In (Map.fromList [(mkVar (Pos 32 35) "B",Skip (Pos 32 38))]))),(mkVar (Pos 32 45) "B",Skip (Pos 32 48))])))])) (Match (Pos 34 3) (ProgVar (Pos 34 9) (mkVar (Pos 34 9) "18_c")) (Map.fromList [(mkVar (Pos 35 5) "A",([mkVar (Pos 35 7) "20_c"],App (Pos 35 12) (TypeApp (Pos 35 12) (mkVar (Pos 35 12) "server'") [Skip (Pos 35 20)]) (ProgVar (Pos 35 26) (mkVar (Pos 35 26) "20_c"))))]))),(mkVar (Pos 39 1) "server'",Lambda (Pos 40 9) Un (mkVar (Pos 40 9) "23_c") (Semi (Pos 39 68) (Rec (Pos 39 29) (TypeVarBind (Pos 39 33) (mkVar (Pos 39 33) "24_x") (Kind (Pos 39 35) Session Lin)) (Choice (Pos 39 39) In (Map.fromList [(mkVar (Pos 39 41) "A",Semi (Pos 39 45) (TypeVar (Pos 39 44) (mkVar (Pos 39 44) "24_x")) (Choice (Pos 39 47) In (Map.fromList [(mkVar (Pos 39 49) "B",Skip (Pos 39 52))]))),(mkVar (Pos 39 59) "B",Skip (Pos 39 62))]))) (TypeVar (Pos 39 70) (mkVar (Pos 39 70) "21_\945"))) (Match (Pos 41 3) (ProgVar (Pos 41 9) (mkVar (Pos 41 9) "23_c")) (Map.fromList [(mkVar (Pos 42 5) "A",([mkVar (Pos 42 7) "25_c"],UnLet (Pos 43 8) (mkVar (Pos 43 12) "26_c") (App (Pos 43 16) (TypeApp (Pos 43 16) (mkVar (Pos 43 16) "server'") [Semi (Pos 43 34) (Choice (Pos 43 24) In (Map.fromList [(mkVar (Pos 43 26) "B",Skip (Pos 43 29))])) (TypeVar (Pos 43 36) (mkVar (Pos 43 36) "21_\945"))]) (ProgVar (Pos 43 39) (mkVar (Pos 43 39) "25_c"))) (Match (Pos 44 8) (ProgVar (Pos 44 14) (mkVar (Pos 44 14) "26_c")) (Map.fromList [(mkVar (Pos 45 10) "B",([mkVar (Pos 45 12) "27_c"],ProgVar (Pos 45 17) (mkVar (Pos 45 17) "27_c")))])))),(mkVar (Pos 47 5) "B",([mkVar (Pos 47 7) "28_c"],ProgVar (Pos 48 7) (mkVar (Pos 48 7) "28_c")))])))]


