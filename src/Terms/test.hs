x = 3 + 2 + 4
x :: Bool


k :: Bool -> Bool -> Bool
k x y = x && True


-- comment
y :: Int -> Int
y a = a rem (2 mod 2)

w :: Int -> (Int, Bool) -> Int
-- w a = (a+2,True)
w a z = 2+2

-- q :: Int
-- q = let x,y = 2+2 in 3

z :: Int -> (!Int;Skip) -> Skip
z x y = 2
