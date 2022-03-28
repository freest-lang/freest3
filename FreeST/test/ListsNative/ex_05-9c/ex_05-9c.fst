-- V exercise 9c

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int]
foldl' f list acc = 
    case list of {
        [] -> acc,
        x :: rest -> foldl' f rest (f x acc)
    }

aplica : [(Int -> Int)] -> [Int] -> [Int]
aplica fs list = foldl' (aplicaFs fs) list []

aplicaFs : [(Int -> Int)] -> Int -> [Int] -> [Int]
aplicaFs fs x acc =
    case fs of {
        [] -> addLast x acc,
        f :: rest -> aplicaFs rest (f x) acc
    }

addLast : Int -> [Int] -> [Int]
addLast x list = 
    case list of {
        [] -> [x],
        y :: rest -> y :: (addLast x rest)
    }

add1 : Int -> Int
add1 x = x + 1

main : [Int] 
main = aplica ([add1,add1]) ([1,2,3])
-- result = [3,4,5]