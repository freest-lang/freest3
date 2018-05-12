module CodeGen.CodeGen1
 ( 
 ) where 

import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as Map
import           Terms.Terms
import  Types.Types -- TODO: Delete
  
-- 1st Pass to check which are the monadic functions
type MonadicMap = Map.Map Expression Bool

-- type ExpEnv = Map.Map TermVar (Params, Expression)
isMonadicEnv :: ExpEnv -> MonadicMap
isMonadicEnv = fst . Map.foldl (\acc x -> isMonadic (fst acc) (snd x)) (Map.empty, False)
                                                                     
isMonadic :: MonadicMap -> Expression -> (MonadicMap, Bool)
isMonadic m Unit = (Map.insert Unit False m, False)
isMonadic m (Integer i) = (Map.insert (Integer i) False m, False)
isMonadic m (Character c) = (Map.insert (Character c) False m, False)
isMonadic m (Boolean b) = (Map.insert (Boolean b) False m, False)
isMonadic m (Variable p x) = (Map.insert (Variable p x) False m, False)
isMonadic m (UnLet p x e1 e2) =
  let (m1, b1) = isMonadic m e1
      (m2, b2) = isMonadic m1 e2 in
      (Map.insert (UnLet p x e1 e2) (b1 || b2) m2, b1 || b2)
     
isMonadic m (App p e1 e2) = 
  let (m1, b1) = isMonadic m e1
      (m2, b2) = isMonadic m1 e2 in
      (Map.insert (App p e1 e2) False m2, b1 || b2)

isMonadic m (TypeApp p e ts) = 
  let (m1, b1) = isMonadic m e in
      (Map.insert (TypeApp p e ts) b1 m1, b1)

isMonadic m (Conditional p e1 e2 e3) =
  let (m1, _) = isMonadic m e1
      (m2, _) = isMonadic m1 e2
      (m3, b) = isMonadic m2 e3 in
    (Map.insert (Conditional p e1 e2 e3) b m3, b)
      
isMonadic m (Pair p e1 e2)  = 
  let (m1, _) = isMonadic m e1
      (m2, _) = isMonadic m1 e2 in
      (Map.insert (Pair p e1 e2) False m2, False)

isMonadic m (BinLet p x y e1 e2)  = 
  let (m1, b1) = isMonadic m e1
      (m2, b2) = isMonadic m1 e2 in
      (Map.insert (BinLet p x y e1 e2) (b1 || b2) m2, b1 || b2)
      
isMonadic m (New p t) = (Map.insert (New p t) True m, True)

isMonadic m (Send p e1 e2) =
  let (m1, _) = isMonadic m e1
      (m2, _) = isMonadic m1 e2 in
      (Map.insert (Send p e1 e2) True m2, True)
      
isMonadic m (Receive p e) =  
  let (m1,_) = isMonadic m e in
      (Map.insert (Receive p e) True m1, True)
      
isMonadic m (Select p x e) =  
  let (m1,_) = isMonadic m e in
      (Map.insert (Select p x e) True m1, True)
      
isMonadic m (Match p e mmap) =
  let (m1, _) = isMapMonadic m mmap in
      (Map.insert (Match p e mmap) True m1, True)

isMonadic m (Fork p e) = (Map.insert (Fork p e) True m, True)

isMonadic m (Constructor p x) = (Map.insert (Constructor p x) False m, False)

isMonadic m (Case p e cm) = 
  let (m1, _) = isMapMonadic m cm in
      (Map.insert (Case p e cm) False m, False) 
  
isMapMonadic :: MonadicMap -> Map.Map a (b, Expression) -> (MonadicMap, Bool)
isMapMonadic m mmap = head $
  Map.foldl (\acc (_, e) -> acc ++ [isMonadic m e]) [] mmap




-- TESTING

test :: ExpEnv -> MonadicMap
test = isMonadicEnv

t1 = Map.fromList [("f1", ([], (UnLet (12,11) "x" (Send (12,20) (Integer 5) (Variable (0,0) "c"))) (Variable (0,0) "x")))]

t2 = Map.fromList [("f",([],BinLet (3,7) "x" "y" (Receive (3,22) (Variable (3,22) "c")) (UnLet (4,7) "z" (Send (4,16) (Variable (4,16) "x") (Variable (4,18) "y")) Unit)))]

t4 = Map.fromList [("avg",([],App (12,7) (App (12,7) (Variable (12,7) "div") (App (0,0) (App (0,0) (Variable (0,0) "(+)") (App (0,0) (App (0,0) (Variable (0,0) "(+)") (App (0,0) (App (0,0) (Variable (0,0) "(+)") (App (0,0) (App (0,0) (Variable (0,0) "(+)") (Integer 1)) (Integer 2))) (Integer 3))) (Integer 4))) (Integer 5))) (Integer 5))),("start",([],Variable (9,9) "avg"))]

t5 = Map.fromList [("a",([],Integer 2)),("b",([],App (0,0) (App (0,0) (Variable (0,0) "(+)") (Integer 2)) (Integer 2))),("boolServer",(["c"],Match (8,9) (Variable (8,9) "c") (Map.fromList [("And",("c1",BinLet (10,11) "n1" "c2" (Receive (10,28) (Variable (10,28) "c1")) (BinLet (11,11) "n2" "c3" (Receive (11,28) (Variable (11,28) "c2")) (UnLet (12,11) "x" (Send (12,20) (App (0,0) (App (0,0) (Variable (0,0) "(&&)") (Variable (12,21) "n1")) (Variable (12,27) "n2")) (Variable (12,31) "c3")) Unit)))),("Not",("c1",BinLet (22,11) "n1" "c2" (Receive (22,28) (Variable (22,28) "c1")) (UnLet (23,11) "x" (Send (23,20) (App (0,0) (Variable (0,0) "not") (Variable (23,25) "n1")) (Variable (23,29) "c2")) Unit))),("Or",("c1",BinLet (16,11) "n1" "c2" (Receive (16,28) (Variable (16,28) "c1")) (BinLet (17,11) "n2" "c3" (Receive (17,28) (Variable (17,28) "c2")) (UnLet (18,11) "x" (Send (18,20) (App (0,0) (App (0,0) (Variable (0,0) "(||)") (Variable (18,21) "n1")) (Variable (18,27) "n2")) (Variable (18,31) "c3")) Unit))))]))),("c",([],Pair (49,6) (Integer 2) (Boolean False))),("client1",(["w"],UnLet (35,7) "w1" (Select (35,19) "And" (Variable (35,23) "w")) (UnLet (36,7) "w2" (Send (36,17) (Boolean True) (Variable (36,22) "w1")) (UnLet (37,7) "r1" (Send (37,17) (Boolean False) (Variable (37,23) "w2")) (BinLet (38,7) "x" "r2" (Receive (38,23) (Variable (38,23) "r1")) (Variable (39,3) "x")))))),("start",([],BinLet (28,7) "w" "r" (New (28,17) (Choice Internal (Map.fromList [("And",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip))),("Not",Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)),("Or",Semi (Message Out BoolType) (Semi (Message Out BoolType) (Semi (Message In BoolType) Skip)))]))) (UnLet (29,7) "x" (Fork (29,16) (App (29,17) (Variable (29,17) "boolServer") (Variable (29,28) "r"))) (App (30,3) (Variable (30,3) "client1") (Variable (30,11) "w")))))]
