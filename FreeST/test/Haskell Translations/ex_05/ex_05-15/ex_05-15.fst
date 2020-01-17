-- V exercise 15

-- curry' :: ((a, b)-> c)-> a -> b ->c
-- uncurry' :: (a -> b -> c)-> (a, b)-> c

data Tuple = T Int Int

curry' : (Tuple -> Int) -> Int -> Int -> Int
curry' f x y = f (T x y)

uncurry' : (Int -> Int -> Int) -> Tuple -> Int
uncurry' f tuple =
    case tuple of {
        T x y -> f x y
    }

add : Int -> Int -> Int
add x y = x + y

main : Bool
main = (curry' (uncurry' add) 10 20) == (uncurry' add (T 10 20))
--result = True