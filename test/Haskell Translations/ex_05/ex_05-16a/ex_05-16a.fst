-- V exercise 16a

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

list : IntList
list = List 1 (List 2 (List 3 (List 4 End)))

main : Int
main = foldr' (\y :Int -> (\z :Int -> y*3 + z)) list 0
--result = 30