-- V exercise 4a

data IntList = E | Lst Int IntList
data IntTupleList = End | List IntTuple IntTupleList
data IntTuple = Tuple Int Int

-- again, there are no polymorphic data types, so this may not do much sense
zipWith' : (Int -> Int -> IntTuple) -> IntList -> IntList -> IntTupleList
zipWith' f list1 list2 =
    case list1 of {
        E -> End,
        Lst x rest1 -> case list2 of {
                            E -> End,
                            Lst y rest2 -> List (f x y) (zipWith' f rest1 rest2)
                        } 
    }

merger : Int -> Int -> IntTuple
merger x y = Tuple x y

main : IntTupleList
main = zipWith' merger (Lst 1 (Lst 2 E)) (Lst 3 (Lst 4 (Lst 5 E)))
-- result = List (Tuple 1 3) (List (Tuple 2 4) End)