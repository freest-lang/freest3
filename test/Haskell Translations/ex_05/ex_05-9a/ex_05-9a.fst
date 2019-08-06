-- V exercise 9a

data IntList = End | List Int IntList
data FuncList = E | L (Int -> Int) FuncList

aplica : FuncList -> IntList -> IntList
aplica fs list =
    case fs of {
        E -> list,
        L f rest -> aplica rest (map' f list)
    }

map' : (Int -> Int) -> IntList -> IntList
map' f list =
    case list of {
        End -> End,
        List x rest -> List (f x) (map' f rest)
    }

add1 : Int -> Int
add1 x = x+1

main : IntList
main = aplica (L add1 (L add1 E)) (List 1 (List 2 (List 3 End)))
-- result = List 3 (List 4 (List 5 End))