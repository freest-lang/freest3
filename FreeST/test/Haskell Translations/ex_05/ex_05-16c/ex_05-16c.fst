-- V exercise 16c

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
function x y = x*x + y

list : IntList
list = List 2 (List 3 (List 4 (List 5 End)))

main : Int
main = foldr' (\x :Int -> (\y :Int -> x*x + y)) list 0
--result = 54
