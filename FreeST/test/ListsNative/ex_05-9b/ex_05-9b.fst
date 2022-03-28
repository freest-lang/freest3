-- V exercise 9b

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int]
foldr' f list acc = foldl' f (reverseIntList list []) acc

reverseIntList : [Int] -> [Int] -> [Int]
reverseIntList list acc =
    case list of {
        [] -> acc,
        x :: rest ->  reverseIntList rest (x :: acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int]
foldl' f list acc = 
    case list of {
        [] -> acc,
        x :: rest -> foldl' f rest (f x acc)
    }

aplica : [(Int -> Int)] -> [Int] -> [Int]
aplica fs list = foldr' (aplicaFs fs) list []

aplicaFs : [(Int -> Int)] -> Int -> [Int] -> [Int]
aplicaFs fs x acc =
    case fs of {
        [] -> x :: acc,
        f :: rest -> aplicaFs rest (f x) acc
    }

add1 : Int -> Int
add1 x = x + 1

main : [Int] 
main = aplica ([add1,add1]) ([1,2,3])
-- result = [3,4,5]