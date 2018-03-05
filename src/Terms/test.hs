-- zed :: Bool -> Bool
-- zed y = not y 

-- x a x = a + a
-- x :: Int -> Int -> Int

-- qq = 3 + 2 + 4
-- qq :: Bool

-- k :: Bool
-- k = True && True


-- -- comment
-- y :: Int -> Int
-- y a = a rem (2 mod 2)

-- w :: Int -> (Int, Bool) -> Int
-- -- w a = (a+2,True)
-- w a z = 2+2

-- -- q :: Int
-- -- q = let x,y = 2+2 in 3

-- z :: Int -> (!Int;Skip) -> Skip
-- z x y = 2

-- iff :: Bool
-- iff = if True then True else False

-- -- parseNew
-- newa :: Int
-- newa = new !Int

-- type Cc = Int

-- data Aa = AA Int -> Bool
--         | CC Char Bool

-- -- parseSend
-- parseSend :: (!Int;Skip) -> Skip
-- parseSend x = send 2 x

-- parseCase :: Int
-- parseCase =
--   case 2+2 of
--     Node -> 23
--     -- D a -> 24
--     -- E a -> 25

-- -- parseReceive
-- parseReceive :: (?Int;Skip) -> Skip
-- parseReceive x = receive x

-- -- parseSelect
-- parseSelect :: Int
-- parseSelect = select C 2

-- data IntMaybe = Nothing | Just Int

-- -- parseFork
-- parseFork :: Int
-- parseFork = fork -2

data Tree =
    Leaf
  | Node Int Tree Tree

-- testConstruct :: Int -> Tree -> Tree -> Tree
-- testConstruct = Node

data C = A Char | B Int Char Bool | C

aaa :: C -> Int
aaa x = case x of 
          B a b c -> b
          A i -> i
          C -> 'C'
