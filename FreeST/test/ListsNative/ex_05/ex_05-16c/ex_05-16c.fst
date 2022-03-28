-- V exercise 16c

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

function : Int -> Int -> Int
function x y = x*x + y

list : [Int]
list = [2,3,4,5]

main : Int
main = foldr' (\x :Int -> (\y :Int -> x*x + y)) list 0
--result = 54