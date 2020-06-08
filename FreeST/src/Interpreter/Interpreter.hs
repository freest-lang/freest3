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
import Interpreter.Value
-- import Interpreter.Eval

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


import Interpreter.Value
import Interpreter.Builtin
import Interpreter.Eval

------------------------------------------------------------
-- TESTS
------------------------------------------------------------

venv :: VarEnv
venv = Map.fromList [ (mkVar defaultPos "f", fromType $ (Basic defaultPos BoolType))
                    , (mkVar defaultPos "h", fromType $ (Basic defaultPos BoolType))
                    ]

var :: String -> ProgVar
var = mkVar defaultPos

t1 :: IO ()
t1 = do
  let s = parseDefs "" venv "f = 2 + 2"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t1")
  res <- eval initialCtx state e
  putStrLn $ show res

t2 :: IO ()
t2 = do
  let s = parseDefs "" venv "f = h 2"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")  
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t2")
  res <- eval initialCtx state e
  putStrLn $ show res  

t3 :: IO ()
t3 = do
  let s = parseDefs "" venv "f = 21 + (k 1 2)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t3")
  res <- eval initialCtx state e
  putStrLn $ show res
 

t4 :: IO ()
t4 = do
  let s = parseDefs "" venv "f = 3 == 1 || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t4")
  res <- eval initialCtx state e
  putStrLn $ show res
  
t5 :: IO ()
t5 = do
  let s = parseDefs "" venv "f = 3 == three || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t5")
  res <- eval initialCtx state e
  putStrLn $ show res

t6 :: IO ()
t6 = do
  let s = parseDefs "" venv "f = 3 == (h 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t6")
  res <- eval initialCtx state e
  putStrLn $ show res

t7 :: IO ()
t7 = do
  let s = parseDefs "" venv "f = 3 == (s 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t7")
  res <- eval initialCtx state e
  putStrLn $ show res  

t8 :: IO ()
t8 = do
  let s = parseDefs "" venv "f = 0 == (m 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t8")
  res <- eval initialCtx state e
  putStrLn $ show res

t9 :: IO ()
t9 = do
  let s = parseDefs "" venv "f = isZero (m 1 1) || False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t9")
  res <- eval initialCtx state e
  putStrLn $ show res


t10 :: IO ()
t10 = do
  let s = parseDefs "" venv "f = (1, 22)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t10")
  res <- eval initialCtx state e
  putStrLn $ show res

t11 :: IO ()
t11 = do
  let s = parseDefs "" venv "f = h (1, 22)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t11")
  res <- eval initialCtx state e
  putStrLn $ show res

-- bin let

t12 :: IO ()
t12 = do
  let s = parseDefs "" venv "f = let (x, y) = h (1, 22) in x + y"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")  
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t12")
  res <- eval initialCtx state e
  putStrLn $ show res
  

-- un let

t13 :: IO ()
t13 = do
  let s = parseDefs "" venv "f = let x = h (1, 22) in x"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t13")
  res <- eval initialCtx state e
  putStrLn $ show res
 



-- un let (twice)

t14 :: IO ()
t14 = do
  let s = parseDefs "" venv "f = let x = fst (1, 22) in let y = snd (1, 22) in x + y"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t14")
  res <- eval initialCtx state e
  putStrLn $ show res
 


t15 :: IO ()
t15 = do
  let s = parseDefs "" venv "f = let (x, y) = (1, 22) in (y, x)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t15")
  res <- eval initialCtx state e
  putStrLn $ show res
 
-- conditional
t16 :: IO ()
t16 = do
  let s = parseDefs "" venv "f = if 0 == 1 then True else False"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t16")
  res <- eval initialCtx state e
  putStrLn $ show res
 
t17 :: IO ()
t17 = do
  let s = parseDefs "" venv "f = half10 12"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval initialCtx state e
  putStrLn $ show res
 
t18 :: IO ()
t18 = do
  let s = parseDefs "" venv "f = half10 (2*3+(5-4)*6+4/2)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval initialCtx state e
  putStrLn $ show res
 
t19 :: IO ()
t19 = do
  let s = parseDefs "" venv "f = ()"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval initialCtx state e
  putStrLn $ show res

t20 :: IO ()
t20 = do
  let s = parseDefs "" venv "f = Cons 2 (Cons 3 Nil)"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval initialCtx state e
  putStrLn $ show res


-- case
t21 :: IO ()
t21 = do
  let s = parseDefs "" venv "f = case cons of {Cons x y -> 0, Nil -> -1}"
  let s = parseDefs "" venv "f = case nil of {Cons x y -> 0, Nil -> -1}"
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval initialCtx state e
  putStrLn $ show res
  



t22 :: IO ()
t22 = do
  let s = parseDefs "" venv "f = let x = Cons 2 (Cons 3 Nil) in case x of {Cons x y -> 0, Nil -> -1}"  
  -- let s = parseDefs "" venv "f = let x = Nil in case x of {Cons x y -> 0, Nil -> -1}"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, _) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval initialCtx state e
  putStrLn $ show res
  



t23 :: IO ()
t23 = do
  let s = parseDefs "" venv "f = recCall 1"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, s) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval initialCtx state e
  putStrLn $ show res
  


-- simple fork

t24 :: IO ()
t24 = do
  let s = parseDefs "" venv "f = fork (1 + 1)"  
  let e = (expEnv s) Map.! (mkVar defaultPos "f")
  let (state, s) = runState (tMapM (rename Map.empty) builtIn) (initialState "t17")
  res <- eval initialCtx state e
  putStrLn $ show res
  -- case res of
  --   IOValue io -> io >>= \res -> putStrLn $ show res
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
  res <- eval initialCtx (expEnv s4) ((expEnv s4) Map.! (mkVar defaultPos "main"))
  putStrLn $ show res
--   case res of
--     IOValue io -> io >>= \res -> putStrLn $ show res
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

