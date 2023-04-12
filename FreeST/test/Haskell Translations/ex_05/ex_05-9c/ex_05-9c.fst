-- V exercise 9c

data IntList = End | List Int IntList
data FuncList = E | L (Int -> Int) FuncList

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldl' f list acc = 
    case list of {
        End -> acc,
        List x rest -> foldl' f rest (f x acc)
    }

aplica : FuncList -> IntList -> IntList
aplica fs list = foldl' (aplicaFs fs) list End

aplicaFs : FuncList -> Int -> IntList -> IntList
aplicaFs fs x acc =
    case fs of {
        E -> addLast x acc,
        L f rest -> aplicaFs rest (f x) acc
    }

addLast : Int -> IntList -> IntList
addLast x list = 
    case list of {
        End -> List x End,
        List y rest -> List y (addLast x rest)
    }

add1 : Int -> Int
add1 x = x + 1

main : IntList 
main = aplica (L add1 (L add1 E)) (List 1 (List 2 (List 3 End)))
-- result = List 3 (List 4 (List 5 End))
