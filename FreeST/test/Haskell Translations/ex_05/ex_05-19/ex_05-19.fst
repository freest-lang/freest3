-- V exercise 19

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

binary2decimal : IntList -> Int
binary2decimal list = 
    case foldr' function list (T 0 0) of {
        T x _ -> x
    }

function : Int -> Tuple -> Tuple
function x tuple = 
    case tuple of {
        T acc i -> T (x*(pow 2 i)+acc) (i+1)
    }

pow : Int -> Int -> Int
pow b e = if e <= 0 then 1 else b * (pow b (e-1))

binary : IntList
binary = (List 1 (List 1 (List 0 (List 1 End))))

main : Int
main = binary2decimal binary
--result = 13
