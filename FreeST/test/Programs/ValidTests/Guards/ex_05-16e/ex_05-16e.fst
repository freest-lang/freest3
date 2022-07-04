-- V exercise 16e

data IntList = End | List Int IntList

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldr' f list acc = foldl' f (reverseIntList list End) acc

reverseIntList : IntList -> IntList -> IntList
reverseIntList End           acc = acc
reverseIntList (List x rest) acc = reverseIntList rest (List x acc)

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldl' f End           acc = acc
foldl' f (List x rest) acc = foldl' f rest (f x acc)

function : Int -> IntList -> IntList
function x s 
    | x == 2    = List x s 
    | otherwise = s

list : IntList
list = List 0 (List 2 (List 4 (List 1 (List 0 (List 5 (List 2 End))))))

main : IntList
main = foldr' function list End
--result = List 2 (List 2 End)