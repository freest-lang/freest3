-- V exercise 16b

data IntList = End | List Int IntList

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> Int -> Int) -> IntList -> Int -> Int
foldr' f list acc = foldl' f (reverseIntList list End) acc

reverseIntList : IntList -> IntList -> IntList
reverseIntList list acc =
    case list of {
        End -> acc,
        List x rest ->  reverseIntList rest (List x acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> Int -> Int) -> IntList -> Int -> Int
foldl' f list acc = 
    case list of {
        End -> acc,
        List x rest -> foldl' f rest (f x acc)
    }

function : Int -> Int -> Int
function x y = if x > 0 then x + y else y 

list : IntList
list = List 4 (List (-3) (List 2 (List (-1) End)))

main : Int
main = foldr' function list 0
--result = 6