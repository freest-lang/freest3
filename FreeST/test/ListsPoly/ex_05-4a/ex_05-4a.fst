-- V exercise 4a

-- again, there are no polymorphic data types, so this may not do much sense
zipWith' : (Int -> Int -> (Int,Int)) -> [Int] -> [Int] -> [(Int,Int)]
zipWith' f list1 list2 =
    case list1 of {
        [] -> [],
        x :: rest1 -> 
            case list2 of {
                [] -> [],
                y :: rest2 -> (f x y) :: (zipWith' f rest1 rest2)
            } 
    }

merger : Int -> Int -> IntTuple
merger x y = (x,y)

main : IntTupleList
main = zipWith' merger ([1,2]) ([3,4,5])
-- result = [(1, 3),(2, 4)]