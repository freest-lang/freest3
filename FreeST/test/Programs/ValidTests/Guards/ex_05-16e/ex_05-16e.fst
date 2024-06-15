-- V exercise 16e

data IntList = Nil | List Int IntList

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldl' f Nil           acc = acc
foldl' f (List x rest) acc = foldl' f rest (f x acc)

reverseIntList : IntList -> IntList -> IntList
reverseIntList Nil           acc = acc
reverseIntList (List x rest) acc = reverseIntList rest (List x acc)

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldr' f list acc = foldl' f (reverseIntList list Nil) acc

function : Int -> IntList -> IntList
function x s 
    | x == 2    = List x s 
    | otherwise = s

list : IntList
list = List 0 (List 2 (List 4 (List 1 (List 0 (List 5 (List 2 Nil))))))

main : IntList
main = foldr' function list Nil
--result = List 2 (List 2 Nil)
