-- V exercise 3e

data IntList = End | List Int IntList
data IntListList = LEnd | LList IntList IntListList

pow2 : Int -> Int
pow2 x = x*x

map' : (IntList -> IntList) -> IntListList -> IntListList
map' f list =
    case list of {
        LEnd -> LEnd,
        LList intList rest -> LList (f intList) (map' f rest)
    }

-- becase there's no polymorphic datas there's the need to make this method
map'' : (Int -> Int) -> IntList -> IntList
map'' f list =
    case list of {
        End -> End,
        List x rest -> List (f x) (map'' f rest)
    }

list1 : IntList
list1 = List 1 (List 2 End)
list2 : IntList
list2 = List 3 (List 4 (List 5 End))

list : IntListList
list = LList list1 (LList list2 LEnd)

main : IntListList
main = map' (map'' pow2) list
-- result = LList (List 1 (List 4 End)) (LList (List 9 (List 16 (List 25 End))) LEnd)