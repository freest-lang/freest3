-- V exercise 9b

data IntList = End | List Int IntList
data FuncList = E | L (Int -> Int) FuncList

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldr' f list acc = foldl' f (reverseIntList list End) acc

reverseIntList : IntList -> IntList -> IntList
reverseIntList list acc =
    case list of {
        End -> acc,
        List x rest ->  reverseIntList rest (List x acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldl' f list acc = 
    case list of {
        End -> acc,
        List x rest -> foldl' f rest (f x acc)
    }

aplica : FuncList -> IntList -> IntList
aplica fs list = foldr' (aplicaFs fs) list End

aplicaFs : FuncList -> Int -> IntList -> IntList
aplicaFs fs x acc =
    case fs of {
        E -> List x acc,
        L f rest -> aplicaFs rest (f x) acc
    }

add1 : Int -> Int
add1 x = x + 1

main : IntList 
main = aplica (L add1 (L add1 E)) (List 1 (List 2 (List 3 End)))
-- result = List 3 (List 4 (List 5 End))