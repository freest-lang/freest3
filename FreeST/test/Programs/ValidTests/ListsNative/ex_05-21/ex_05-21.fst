-- V exercise 21

foldr' : (Int -> (Int,Int) -> (Int,Int)) -> [Int] -> (Int,Int) -> (Int,Int)
foldr' f list acc = foldl' f (reverseIntList list []) acc

reverseIntList : [Int] -> [Int] -> [Int]
reverseIntList list acc =
    case list of {
        [] -> acc,
        x :: rest ->  reverseIntList rest (x :: acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> (Int,Int) -> (Int,Int)) -> [Int] -> (Int,Int) -> (Int,Int)
foldl' f list acc = 
    case list of {
        [] -> acc,
        x :: rest -> foldl' f rest (f x acc)
    }

poly : Int -> [Int] -> Int
poly x list = 
    let (t,_) = foldr' (function x) list (0,0) in t

function : Int -> Int -> (Int,Int) -> (Int,Int)
function x n tuple =
    let (acc,i) = tuple in (((n*(pow x i)) + acc),(i+1))

pow : Int -> Int -> Int
pow b e = if e <= 0 then 1 else b * (pow b (e-1))

list : [Int]
list = [5,2,0,1,2]

main : Int
main = poly 2 list
--result = 100