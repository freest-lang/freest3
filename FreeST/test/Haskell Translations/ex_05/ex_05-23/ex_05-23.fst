-- V exercise 23

data IntList = E | L Int IntList
data IntListList = End | List IntList IntListList

data BoolList = BE | BL Bool BoolList
data BoolListList = BEnd | BList BoolList BoolListList

gz : IntListList -> BoolListList
gz list =
    case list of {
        End -> BEnd,
        List l rest -> BList (gz' l) (gz rest)
    }

gz' : IntList -> BoolList
gz' list =
    case list of {
        E -> BE,
        L x rest -> BL (x > 0) (gz' rest)
    }

list1 : IntList
list1 = L 1 (L 2 (L 3 E))
list2 : IntList
list2 = L 2 (L (-1) (L 3 (L 7 E)))

list : IntListList
list = List list1 (List list2 End)

main : BoolListList
main = gz list
--result = BList (BL True (BL True (BL True BE))) (BList (BL True (BL False (BL True (BL True BE)))) BEnd)