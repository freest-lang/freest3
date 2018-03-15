-- zed :: Bool -> Bool
-- zed y = not y 

-- x a x = a + a
-- x :: Int -> Int -> Int

-- plas :: Int
-- plas = -10

-- qq = 3 + 2 + 4
-- qq :: Int

-- k :: Bool
-- k = True && True


-- -- comment
-- y :: Int -> Int
-- y a = a rem (2 mod 2)

-- w :: Int -> (Int, Bool) -> Int
-- -- w a = (a+2,True)
-- w a z = 2+2

-- q :: (Int,Int)
-- q = let x,y = (2+2,2) in (3,4)

-- z :: Int -> (!Int;Skip) -> Int
-- z x y = 2

-- iff :: Bool
-- iff = if True then True else False

-- -- parseNew
-- newa :: (!Int, ?Int)
-- newa = new !Int

-- type Cc = Int

-- data Aa = AA Int -> Bool
--         | CC Char Bool

-- --parseSend: TODO: infinite
-- parseSend :: (!Int;Skip) -> Skip
-- parseSend x = send 2 x

-- parseCase :: Int
-- parseCase =
--   case 2+2 of
--     Node -> 23
    -- D a -> 24
    -- E a -> 25

-- parseReceive TODO
-- (?Int;Skip) -> ((?Int;Skip) -> (Int , Skip))
--  ((?Int;Skip) -> (Int , Skip))
-- parseReceive ::  (?Int;Skip) -> (?Int;Skip) 
-- parseReceive x = receive x


-- -- parseSelect
-- -- parseSelect :: Int
-- -- parseSelect = select C 2

-- data IntMaybe = Nothing | Just Int

-- -- parseFork
-- parseFork :: ()
-- parseFork = fork -2

-- data Tree =
--     Leaf
--   | Node Int Int Int
-- --  | Node Int Tree Tree

-- -- testConstruct :: Int -> Tree -> Tree -> Tree
-- -- testConstruct = Node

-- data C = A Char | B Int Char Bool | C

-- aaa :: C -> Char
-- aaa x = case x of 
--           B a b c -> a
--           A i -> i
--           C -> 'C'

-- qqqq :: Int
-- qqqq = 3*(2 - 1)

type MyType = String

start :: Bool
start = fact 12

fact :: Int -> Int
fact n = if n == 0 then 1 else n * fact (n-1)

-- test :: Int -> Int
-- test n = n -1

-- a :: Int
-- a = 2 * b 2-1

-- b :: Int -> Int
-- b x = x
-- --test n = test 1

-- myPair :: (Int,Bool)
-- myPair = (2,True)
