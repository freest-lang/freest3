-- -- -- zed :: Bool -> Bool
-- -- -- zed y = not y 

-- -- -- x a x = a + a
-- -- -- x :: Int -> Int -> Int

-- -- -- unary :: Int
-- -- -- unary = -10

-- -- -- qq = 3 + 2 + 4
-- -- -- qq :: Int

-- -- -- k :: Bool
-- -- -- k = True && True


-- -- -- -- comment
-- -- -- y :: Int -> Int
-- -- -- y a = a rem (2 mod 2)

-- -- -- w :: Int -> (Int, Bool) -> Int
-- -- -- -- w a = (a+2,True)
-- -- -- w a z = 2+2

-- -- -- q :: (Int,Int)
-- -- -- q = let x,y = (2+2,2) in (3,4)

-- -- -- z :: Int -> (!Int;Skip) -> Int
-- -- -- z x y = 2

-- -- -- iff :: Bool
-- -- -- iff = if True then True else False

-- -- -- -- parseNew
-- -- -- newa :: (!Int, ?Int)
-- -- -- newa = new !Int

-- -- -- type Cc = Int

-- -- -- data Aa = AA Int -> Bool
-- -- --         | CC Char Bool

-- -- -- --parseSend: TODO: infinite
-- -- -- parseSend :: Int -> (!Int;Skip) -> Skip
-- -- -- parseSend i x = send i x

-- -- -- parseCase :: Int
-- -- -- parseCase =
-- -- --   case 2+2 of
-- -- --     Node -> 23
-- -- --     D a -> 24
-- -- --     E a -> 25

-- -- -- parseReceive TODO
-- -- -- (?Int;Skip) -> ((?Int;Skip) -> (Int , Skip))
-- -- --  ((?Int;Skip) -> (Int , Skip))
-- -- -- parseReceive ::  (?Int;Skip) -> (?Int;Skip) 
-- -- -- parseReceive x = receive x


-- -- -- -- parseSelect
-- -- -- -- parseSelect :: Int
-- -- -- -- parseSelect = select C 2

-- -- -- data IntMaybe = Nothing | Just Int

-- -- -- -- parseFork
-- -- -- parseFork :: ()
-- -- -- parseFork = 

-- -- -- -- testConstruct :: Int -> Tree -> Tree -> Tree
-- -- -- -- testConstruct = Node

-- -- -- data C = A Char | B Int Char Bool | D

-- -- -- aaa :: C -> Char
-- -- -- aaa x = case x of 
-- -- --           B a b c -> b
-- -- --           A i -> i
-- -- --           D -> 'C'

-- -- -- qqqq :: Int
-- -- -- qqqq = 3*(2 - 1)

-- -- -- type MyType = String

-- -- -- start :: Int
-- -- -- start = fact 12

-- -- -- fact :: Int -> Int
-- -- -- fact n = if n == 0 then 1 else n * fact (n-1)

-- -- -- test :: Int -> Int
-- -- -- test n = n -1

-- -- -- a :: Int
-- -- -- a = 2 * b 2-1

-- -- -- b :: Int -> Int
-- -- -- b x = x
-- -- -- --test n = test 1

-- myPair :: (Int,Bool)
-- myPair = (2,True)

-- asd :: Int -> Int 
-- --asd :: Int -> Int 
-- asd aree = 2 + 2


-- data Tree =
--     Leaf
--   | Node Int Tree Tree

-- data Treea =
--     Leafa
--   | Nodea Int Tree Tree

-- start :: Int
-- start =
--   let w, r = new !Int;Skip in
--   let w1 = fork (send 5 w) in
--   let n, r1 = receive r in
--   n

-- start :: Int
-- start = succ' 1

-- succ' :: Int -> Int
-- succ' x = x + 1

-- Type-safe serialization of a binary tree
-- sendTree :: forall a :: SU => Tree -> rec x::SU.+{Leaf: Skip, Node: !Int;x;x} ; a -> a
-- sendTree :: Tree -> rec x.+{LeafC: Skip, NodeC: !Int;x;x} ; a -> a
-- sendTree t c =
--   case t of
--     Leaf ->
--        select LeafC c
--     Node x l r ->
--       let c1 = select NodeC c in
--       let c2 = send x c1 in
--       let c3 = sendTree[rec x.+{LeafC: Skip, NodeC: !Int;x;x} ; a] l c2 in
--       let c4 = sendTree[a] r c3 in
--       c4

a :: Int -> Bool
a x = if y > 23 then True else False

b :: Bool
b = a 2

start :: Bool
start = b
