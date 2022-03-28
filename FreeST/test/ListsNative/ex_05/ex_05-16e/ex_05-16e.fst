-- V exercise 16e

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

function : Int -> [Int] -> [Int]
function x s = if x == 2 then x :: s else s

list : [Int]
list = [0,2,4,1,0,5,2]

main : [Int]
main = foldr' function list []
--result = [2,2]