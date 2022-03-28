-- V exercise 18

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

map' : (Int -> Int) -> [Int] -> [Int]
map' f list = foldr' (mapper f) list []

mapper : (Int -> Int) -> Int -> [Int] -> [Int]
mapper f x acc = (f x) :: acc

filter' : (Int -> Bool) -> [Int] -> [Int]
filter' f list = foldr' (filtering f) list []

filtering : (Int -> Bool) -> Int -> [Int] -> [Int]
filtering f x acc = if f x then x :: acc else acc

isPositive : Int -> Bool
isPositive x = x > 0

squareIt : Int -> Int
squareIt x = x*x

list : [Int]
list = [-2,-1,0,1,2]

main : [Int]
main = map' squareIt (filter' isPositive list)
--result = [1,4]