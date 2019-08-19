-- V exercise 21

data IntList = End | List Int IntList
data Tuple = T Int Int

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

poly : Int -> IntList -> Int
poly x list = 
    case foldr' (function x) list (T 0 0) of {
        T t _ -> t
    }

function : Int -> Int -> Tuple -> Tuple
function x n tuple =
    case tuple of {
        T acc i -> T ((n*(pow x i)) + acc) (i+1)
    }

pow : Int -> Int -> Int
pow b e = if e <= 0 then 1 else b * (pow b (e-1))

list : IntList
list = List 5 (List 2 (List 0 (List 1 (List 2 End))))

main : Int
main = poly 2 list
--result = 100