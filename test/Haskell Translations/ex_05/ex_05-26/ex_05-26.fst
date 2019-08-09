-- V exercise 26

data IntList = End | List Int IntList
data Tuple = T Int Int

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> Tuple -> Tuple) -> IntList -> Tuple -> Tuple
foldr' f list acc = foldl' f (reverseIntList list End) acc

reverseIntList : IntList -> IntList -> IntList
reverseIntList list acc =
    case list of {
        End -> acc,
        List x rest ->  reverseIntList rest (List x acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> Tuple -> Tuple) -> IntList -> Tuple -> Tuple
foldl' f list acc = 
    case list of {
        End -> acc,
        List x rest -> foldl' f rest (f x acc)
    }

sumlen : IntList -> Tuple
sumlen list = foldr' sumlen' list (T 0 0)

sumlen' : Int -> Tuple -> Tuple
sumlen' x tuple =
    case tuple of {
        T a b -> T (a+x) (b+1)
    }

list : IntList
list = List 1 (List 2 (List 3 (List 4 End)))

main : Tuple
main = sumlen list
--result = T 10 4