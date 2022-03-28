-- V exercise 19

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
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

binary2decimal : [Int] -> Int
binary2decimal list = 
    let (x,_) = foldr' function list (0,0) in x

function : Int -> (Int,Int) -> (Int,Int)
function x tuple = 
    let (acc,i) = tuple in
    ((x*(pow 2 i)+acc),(i+1))

pow : Int -> Int -> Int
pow b e = if e <= 0 then 1 else b * (pow b (e-1))

binary : [Int]
binary = [1,1,0,1]

main : Int
main = binary2decimal binary
--result = 13