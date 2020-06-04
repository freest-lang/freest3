{-# LANGUAGE FlexibleInstances, BangPatterns #-}
{-# LANGUAGE NoMonadFailDesugaring, LambdaCase, MultiWayIf #-}
module Interpreter.Interpreter where

import qualified Syntax.Expressions as E
import qualified Data.Map as Map
import Syntax.ProgramVariables
import Syntax.Base
import Data.Char (chr, ord)
import Data.List (intercalate)
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as C


-- debug
import Parse.Parser
import Control.Monad.State
import Syntax.Schemes
import Syntax.Types
import Utils.FreestState
import Validation.Rename -- Debug
import System.Exit (die)
import Utils.PreludeLoader
import Validation.TypeChecking
import Validation.BuildTypes
import Debug.Trace


data Value =
    Unit
  | Integer Int
  | Boolean Bool
  | Character Char  
  | Cons ProgVar [[Value]] -- TODO: Think how to do this in other way
  | Pair Value Value
  | Closure ProgVar E.Expression Ctx
  | PrimitiveFun (Value -> Value)
  | Label String -- to be sent over channels
  | Chan ChannelEnd
  | PureWrapper (IO Value)
--  | Send ChannelEnd
  
  -- receive
  

type Ctx = Map.Map ProgVar Value

type ChannelEnd = (C.Chan Value, C.Chan Value)
type Channel    = (ChannelEnd, ChannelEnd)

send :: ChannelEnd -> Value -> IO ChannelEnd
send c v = do
  C.writeChan (snd c) v
  return c

new :: IO Channel
new = do
  ch1 <- C.newChan
  ch2 <- C.newChan
  return ((ch1, ch2), (ch2, ch1))

receive :: ChannelEnd -> IO (Value, ChannelEnd)
receive ch = do
  v <- C.readChan (fst ch)
  return (v, ch)


instance Show Value where
  show Unit          = "()"
  show (Integer i)   = show i
  show (Boolean b)   = show b
  show (Character c) = show c
  show (Label s)     = s
  show (Pair v1 v2)  = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  show c@(Cons _ _)  = showCons c
  show (Chan _)      = "Skip" -- TODO: change this
  show (Closure x e _)  = show x ++ " " ++ show e-- TODO: change this


showCons :: Value -> String
showCons (Cons x []) = show x
showCons (Cons x xs) = show x ++ " " ++ (intercalate " " (map showConstrList xs))
 where
   showConstrList :: [Value] -> String
   showConstrList xs = intercalate " " (map showC xs)

   showC :: Value -> String
   showC c@(Cons _ []) = show c
   showC c@(Cons _ _) = "(" ++ show c ++ ")"
   showC v = show v


------------------------------------------------------------
-- EVALUATION
------------------------------------------------------------

eval :: Ctx -> E.ExpEnv -> E.Expression -> IO Value
eval _ _ (E.Unit _)        = return Unit
eval _ _ (E.Integer _ i)   = return $ Integer i
eval _ _ (E.Boolean _ b)   = return $ Boolean b
eval _ _ (E.Character _ c) = return $ Character c
eval ctx eenv (E.ProgVar _ x) = evalVar ctx eenv x
eval ctx eenv (E.TypeApp _ x _) = evalVar ctx eenv x
eval ctx _ (E.Lambda _ _ x _ e) = return $ Closure x e ctx
eval ctx eenv (E.App _ e1 e2) =
  eval ctx eenv e1 >>= \case
    (Closure x e ctx') -> do
      !v <- eval ctx eenv e2
      eval (Map.insert x v ctx') eenv e
    (PrimitiveFun f) -> do
      !v <- eval ctx eenv e2
      case f v of
         (PureWrapper res) -> do
           !r <- res
           pure $ r
         r -> return r
    (Cons x xs) -> do
      !v <- eval ctx eenv e2
      pure $ Cons x (xs ++ [[v]])

eval ctx eenv (E.Pair _ e1 e2) =
  liftM2 Pair (eval ctx eenv e1) (eval ctx eenv e2)

eval ctx eenv (E.BinLet _ x y e1 e2) = do
  (Pair v1 v2) <- eval ctx eenv e1
  let env = Map.insert x v1 (Map.insert y v2 ctx)
  eval env eenv e2

eval ctx eenv (E.Conditional _ cond e1 e2) = do
  (Boolean b) <- eval ctx eenv cond 
  if b then eval ctx eenv e1 else eval ctx eenv e2
    
eval ctx eenv (E.UnLet _ x e1 e2) = do
  v <- eval ctx eenv e1
  eval (Map.insert x v ctx) eenv e2

eval ctx eenv (E.Case _ e m) =
  eval ctx eenv e >>= evalCase ctx eenv m

eval ctx eenv (E.Fork _ e) = do
  _ <- forkIO $ eval ctx eenv e >> return ()
  return Unit


eval _ _ (E.New _ _ _) = do
  (c1, c2) <- new
  return $ Pair (Chan c1) (Chan c2) 
--  liftM (uncurry Pair) new 

eval ctx eenv (E.Send _ e) = do
  (Chan c) <- eval ctx eenv e
  return $ PrimitiveFun (\v ->
                PureWrapper $ liftM Chan (send c v))
  
eval ctx eenv (E.Receive _ e) = do
   (Chan c) <- eval ctx eenv e
   !(v, c) <- receive c
   return $ Pair v (Chan c)

eval ctx eenv (E.Select _ e x) = do
   (Chan c) <- eval ctx eenv e
   c <- send c (Label (show x))
   return $ Chan c

eval ctx eenv (E.Match _ e m) = do
  (Chan c) <- eval ctx eenv e
  (Label !v, !c) <- receive c 
  let (patterns:_, e) = m Map.! (mkVar defaultPos v)
  let ctx' = Map.insert patterns (Chan c) ctx
  eval ctx' eenv e

  

evalCase :: Ctx -> E.ExpEnv -> E.FieldMap -> Value -> IO Value
evalCase ctx eenv m (Cons x xs) = do
  let !(patterns, e) = m Map.! x
  let lst  = zip patterns xs
  let ctx1 = foldl (\acc (c,xs) -> Map.insert c (head xs) acc) ctx lst  
  eval ctx1 eenv e


evalVar :: Ctx -> E.ExpEnv -> ProgVar -> IO Value
evalVar ctx eenv x
  | isADT x           = return $ Cons x []
  | Map.member x eenv = eval ctx eenv (eenv Map.! x)
  | Map.member x ctx  = return $ ctx Map.! x
  | otherwise         = error $ "error evaluating progvar: " ++ show x



------------------------------------------------------------  
-- SETUP, builtin functions
------------------------------------------------------------
var :: String -> ProgVar
var = mkVar defaultPos

ctxBuiltin :: Ctx 
ctxBuiltin =
  Map.fromList
  [ (var "(+)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x + y)))  
  , (var "(-)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x - y)))
  , (var "(*)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x * y)))
  , (var "(/)" , PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y)))
  , (var "div", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `div` y)))
  , (var "rem", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Integer $ x `rem` y)))
  , (var "(==)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x == y)))
  , (var "(<=)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x <= y)))  
  , (var "(>=)", PrimitiveFun (\(Integer x) -> PrimitiveFun (\(Integer y) -> Boolean $ x >= y)))  
  , (var "(&&)", PrimitiveFun (\(Boolean x) -> PrimitiveFun (\(Boolean y) -> Boolean $ x && y)))
  , (var "(||)", PrimitiveFun (\(Boolean x) -> PrimitiveFun (\(Boolean y) -> Boolean $ x || y)))
  , (var "negate", PrimitiveFun (\(Integer x) -> Integer $ negate x))
  , (var "not", PrimitiveFun (\(Boolean x) -> Boolean $ not x))
  , (var "chr", PrimitiveFun (\(Integer x) -> Character $ chr x))
  , (var "ord", PrimitiveFun (\(Character x) -> Integer $ ord x))
  , (var "printInt", PrimitiveFun (\(Integer x) -> PureWrapper (putStrLn (show x) >> return Unit)))
  , (var "print", PrimitiveFun (\x -> PureWrapper (putStrLn (show x) >> return Unit)))
  ]




------------------------------------------------------------
-- TESTS
------------------------------------------------------------

venv :: VarEnv
venv = Map.fromList [ (mkVar defaultPos "f", fromType $ (Basic defaultPos BoolType))
                    , (mkVar defaultPos "h", fromType $ (Basic defaultPos BoolType))
                    ]


t1 :: IO ()
t1 = do
  let s = parseDefs "" venv "f = 2 + 2"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  res <- eval ctxBuiltin state e
  putStrLn $ show res

t2 :: IO ()
t2 = do
  let s = parseDefs "" venv "f = h 2"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")  
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t2")
  res <- eval ctxBuiltin state e
  putStrLn $ show res  

t3 :: IO ()
t3 = do
  let s = parseDefs "" venv "f = 21 + (k 1 2)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t3")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
 

t4 :: IO ()
t4 = do
  let s = parseDefs "" venv "f = 3 == 1 || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t4")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
  
t5 :: IO ()
t5 = do
  let s = parseDefs "" venv "f = 3 == three || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t5")
  res <- eval ctxBuiltin state e
  putStrLn $ show res

t6 :: IO ()
t6 = do
  let s = parseDefs "" venv "f = 3 == (h 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t6")
  res <- eval ctxBuiltin state e
  putStrLn $ show res

t7 :: IO ()
t7 = do
  let s = parseDefs "" venv "f = 3 == (s 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t7")
  res <- eval ctxBuiltin state e
  putStrLn $ show res  

t8 :: IO ()
t8 = do
  let s = parseDefs "" venv "f = 0 == (m 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t8")
  res <- eval ctxBuiltin state e
  putStrLn $ show res

t9 :: IO ()
t9 = do
  let s = parseDefs "" venv "f = isZero (m 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t9")
  res <- eval ctxBuiltin state e
  putStrLn $ show res


t10 :: IO ()
t10 = do
  let s = parseDefs "" venv "f = (1, 22)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t10")
  res <- eval ctxBuiltin state e
  putStrLn $ show res

t11 :: IO ()
t11 = do
  let s = parseDefs "" venv "f = h (1, 22)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t11")
  res <- eval ctxBuiltin state e
  putStrLn $ show res

-- bin let

t12 :: IO ()
t12 = do
  let s = parseDefs "" venv "f = let (x, y) = h (1, 22) in x + y"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")  
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t12")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
  

-- un let

t13 :: IO ()
t13 = do
  let s = parseDefs "" venv "f = let x = h (1, 22) in x"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t13")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
 



-- un let (twice)

t14 :: IO ()
t14 = do
  let s = parseDefs "" venv "f = let x = fst (1, 22) in let y = snd (1, 22) in x + y"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t14")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
 


t15 :: IO ()
t15 = do
  let s = parseDefs "" venv "f = let (x, y) = (1, 22) in (y, x)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t15")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
 
-- conditional
t16 :: IO ()
t16 = do
  let s = parseDefs "" venv "f = if 0 == 1 then True else False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t16")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
 
t17 :: IO ()
t17 = do
  let s = parseDefs "" venv "f = half10 12"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
 
t18 :: IO ()
t18 = do
  let s = parseDefs "" venv "f = half10 (2*3+(5-4)*6+4/2)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
 
t19 :: IO ()
t19 = do
  let s = parseDefs "" venv "f = ()"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval ctxBuiltin state e
  putStrLn $ show res

t20 :: IO ()
t20 = do
  let s = parseDefs "" venv "f = Cons 2 (Cons 3 Nil)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval ctxBuiltin state e
  putStrLn $ show res


-- case
t21 :: IO ()
t21 = do
  let s = parseDefs "" venv "f = case cons of {Cons x y -> 0, Nil -> -1}"
  let s = parseDefs "" venv "f = case nil of {Cons x y -> 0, Nil -> -1}"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
  



t22 :: IO ()
t22 = do
  let s = parseDefs "" venv "f = let x = Cons 2 (Cons 3 Nil) in case x of {Cons x y -> 0, Nil -> -1}"  
  -- let s = parseDefs "" venv "f = let x = Nil in case x of {Cons x y -> 0, Nil -> -1}"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
  



t23 :: IO ()
t23 = do
  let s = parseDefs "" venv "f = recCall 1"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, s) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
  


-- simple fork

t24 :: IO ()
t24 = do
  let s = parseDefs "" venv "f = fork (1 + 1)"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, s) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval ctxBuiltin state e
  putStrLn $ show res
  -- case res of
  --   PureWrapper io -> io >>= \res -> putStrLn $ show res
  --   _              -> putStrLn $ show res
  

testFromFile :: FilePath -> IO ()
testFromFile args = do
  s1 <- parseProgram args prelude
  when (hasErrors s1) (die $ getErrors s1)
  let s2 = execState renameState s1
  let s3 = execState solveTypeDecls s2
  when (hasErrors s3) (die $ getErrors s3)
  let s4 = execState typeCheck s3
  when (hasErrors s4) (die $ getErrors s4)
  res <- eval ctxBuiltin (expEnv s4) ((expEnv s4) Map.! (mkVar defaultPos "main"))
  putStrLn $ show res
--   case res of
--     PureWrapper io -> io >>= \res -> putStrLn $ show res
--     _              -> putStrLn $ show res
  

t25 :: IO ()
t25 = testFromFile "/home/balmeida/workspaces/ContextFreeSession/tmp/parse.fst"
  

plus, xVar, yVar, zVar  :: ProgVar
plus = mkVar defaultPos "(+)"
xVar = mkVar defaultPos "x"
yVar = mkVar defaultPos "y"
zVar = mkVar defaultPos "z"

intType :: Type
intType = Basic defaultPos IntType 

p' :: Pos
p' = defaultPos


-- builtin + eenv
--builtIn :: Ctx
builtIn :: E.ExpEnv
builtIn =
  Map.fromList
    [
      -- id
      -- h = λ x -> x 
      (var "h", E.Lambda p' Un xVar intType (E.ProgVar p' xVar))
      -- k = λ x -> λ y -> y
    , (var "k", E.Lambda p' Un xVar intType (E.Lambda p' Un yVar intType (E.ProgVar p' yVar)))
      -- s = λ x -> λ y -> x+y
    , (var "s", E.Lambda p' Un xVar intType (E.Lambda p' Un yVar intType
           ((E.App p' (E.App p' (E.ProgVar p' plus) (E.ProgVar p' xVar)) (E.ProgVar p' yVar)))))
      -- x = 3
    , (var "three", E.Integer p' 3) -- test with var three and env
     -- m = λ x -> λ y -> x-y
    , (var "m", E.Lambda p' Un xVar intType (E.Lambda p' Un yVar intType
           (E.App p' (E.App p' (E.ProgVar p' (var "(-)")) (E.ProgVar p' xVar)) (E.ProgVar p' yVar))))
     -- isZero = λ x -> x == 0
    , (var "isZero", E.Lambda p' Un xVar intType
        (E.App p' (E.App p' (E.ProgVar p' (var "(==)")) (E.Integer p' 0)) (E.ProgVar p' xVar)))
     -- fst = λ x -> let (a, b) = x in a
    , (var "fst", E.Lambda p' Un xVar intType
       (E.BinLet p' (var "a") (var "b") (E.ProgVar p' xVar) (E.ProgVar p' (var "a")))
     )
     -- snd = λ x -> let (a, b) = x in b
    , (var "snd", E.Lambda p' Un xVar intType
       (E.BinLet p' (var "a") (var "b") (E.ProgVar p' xVar) (E.ProgVar p' (var "b")))
     )
     -- half10 = λ x -> if x < 10 then x else f (x / 2)
    , (var "half10", E.Lambda p' Un xVar intType
       (E.Conditional p'
         (E.App p' (E.App p' (E.ProgVar p' (var "(<=)")) (E.ProgVar p' xVar)) (E.Integer p' 10))
         (E.ProgVar p' xVar)
         (E.App p' (E.ProgVar p' (var "half10"))
           (E.App p' (E.App p' (E.ProgVar p' (var "(/)")) (E.ProgVar p' xVar)) (E.Integer p' 2)))
      ))
    
   , (var "cons", (E.App p'
                      (E.App p' (E.ProgVar p' (var "Cons")) (E.Integer p' 2))
                      (E.App p'
                         (E.App p' (E.ProgVar p' (var "Cons")) (E.Integer p' 3))
                         (E.ProgVar p' (var "Nil"))))
     )
   , (var "nil", (E.ProgVar p' (var "Nil")))
   
   , (var "recCall", E.Lambda p' Un xVar intType
       (E.Conditional p'
         (E.App p' (E.App p' (E.ProgVar p' (var "(<=)")) (E.ProgVar p' xVar)) (E.Integer p' 10))
         (E.App p' (E.App p' (E.ProgVar p' (var "(*)")) (E.Integer p' 2))
                 (E.App p'
                   (E.ProgVar p' (var "recCall"))
                   (E.App p' (E.App p' (E.ProgVar p' (var "(+)"))
                           (E.Integer p' 1)) (E.ProgVar p' xVar))
                 ))
         (E.Integer p' 1)
        
      ))
    ]





tests :: IO ()
tests = do
  let x = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10
          ,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20
          ,t21,t22,t23,t24,t25
          ]
  sequence_ x

