-- V exercise 16d

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> Int -> Int) -> [Int] -> Int -> Int
foldr' f list acc = foldl' f (reverseIntList list []) acc

reverseIntList : [Int] -> [Int] -> [Int]
reverseIntList list acc =
    case list of {
        [] -> acc,
        x :: rest ->  reverseIntList rest (x :: acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> Int -> Int) -> [Int] -> Int -> Int
foldl' f list acc = 
    case list of {
        [] -> acc,
        x :: rest -> foldl' f rest (f x acc)
    }

list : [Int]
list = [-3,-2,-1]

main : Int
main = foldr' (*) list 1
--result = -6