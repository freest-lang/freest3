-- x a b = b + a
-- x :: Int -> Int -> Int
--
-- qq = 3 + 2 + 4
-- qq :: Bool
--
-- k :: Bool
-- k = True && True
--
--
-- -- comment
-- y :: Int -> Int
-- y a = a rem (2 mod 2)
--
-- w :: Int -> (Int, Bool) -> Int
-- -- w a = (a+2,True)
-- w a z = 2+2
--
-- -- q :: Int
-- -- q = let x,y = 2+2 in 3
--
-- z :: Int -> (!Int;Skip) -> Skip
-- z x y = 2
--
-- iff :: Bool
-- iff = if True then True else False
--
-- -- parseNew
-- newa :: Int
-- newa = new !Int
--
-- type Cc = Int

data Aa = AA Int -> Bool
        | CC Char Bool

-- -- parseSend
-- parseSend :: Int
-- parseSend = send 2 True
--
-- -- parseReceive
-- parseReceive :: Int
-- parseReceive = receive 2
--
-- -- parseSelect
-- parseSelect :: Int
-- parseSelect = select C 2

-- parseFork
parseFork :: Int
parseFork = fork 2
