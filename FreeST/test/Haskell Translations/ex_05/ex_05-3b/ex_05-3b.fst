-- V exercise 3b

data IntList = IEnd | IList Int IntList
data BoolList = BEnd | BList Bool BoolList

map' : (Int -> Bool) -> IntList -> BoolList
map' f list = 
    case list of {
        IEnd -> BEnd,
        IList x rest -> BList (f x) (map' f rest)
    }

isBiggerThen0 : Int -> Bool
isBiggerThen0 x = x > 0

list : IntList
list = IList 1 (IList 2 (IList 3 (IList 4 (IList 5 (IList 6 (IList (-1) IEnd))))))

main : BoolList
main = map' isBiggerThen0 list
-- result = BList True (BList True (BList True (BList True (BList True (BList True (BList False BEnd))))))
