-- V exercise 9a

aplica : [(Int -> Int)] -> [Int] -> [Int]
aplica fs list =
    case fs of {
        [] -> list,
        f :: rest -> aplica rest (map' f list)
    }

map' : (Int -> Int) -> [Int] -> [Int]
map' f list =
    case list of {
        [] -> [],
        x :: rest -> (f x) :: (map' f rest)
    }

add1 : Int -> Int
add1 x = x+1

main : [Int]
main = aplica ([add1,add1]) ([1,2,3])
-- result = [3,4,5]