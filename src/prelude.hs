-- Integer prelude
(+) :: Int -> Int -> Int
(-) :: Int -> Int -> Int
(/) :: Int -> Int -> Int
(*) :: Int -> Int -> Int
mod :: Int -> Int -> Int
rem :: Int -> Int -> Int
negate :: Int -> Int 

not :: Bool -> Bool
(&&) :: Bool -> Bool -> Bool
(||) :: Bool -> Bool -> Bool
(==) :: Int -> Int -> Bool
-- (/=) :: a -> a -> Bool 

-- (<) :: Int -> Int -> Bool
-- (<=) :: Int -> Int -> Bool
-- (>) :: Int -> Int -> Bool
-- (>=) :: Int -> Int -> Bool

-- -- ??
-- max :: Int -> Int -> a
-- min :: Int -> Int -> a 

-- otherwise :: Bool

-- Tuples

-- fst :: (a, b) -> a
-- snd :: (a, b) -> b
-- curry :: ((a, b) -> c) -> a -> b -> c
-- uncurry :: (a -> b -> c) -> (a, b) -> c


-- -- map :: (a -> b) -> [a] -> [b] 
-- -- (++) :: [a] -> [a] -> [a]
-- filter :: (a -> Bool) -> [a] -> [a]
-- head :: [a] -> a
-- last :: [a] -> a
-- tail :: [a] -> [a]
-- init :: [a] -> [a]
-- -- null :: Foldable t => t a -> Bool
-- length :: [a] -> Int
-- reverse :: [a] -> [a] 

-- --Infinite lists

-- iterate :: (a -> a) -> a -> [a]
-- repeat :: a -> [a]
-- replicate :: Int -> a -> [a]
-- cycle :: [a] -> [a]

-- -- Sublists
-- take :: Int -> [a] -> [a]
-- drop :: Int -> [a] -> [a]
-- splitAt :: Int -> [a] -> ([a], [a])
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- span :: (a -> Bool) -> [a] -> ([a], [a])
-- break :: (a -> Bool) -> [a] -> ([a], [a])

-- ...
