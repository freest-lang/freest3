-- VII exercise 4

-- list's lookups are O(n), sisce they are not implemented 
-- so it's not possible to get much of a complexity improvement

data List = E | L Int Int List
data Map = Map List

empty : Map -> Bool
empty m = 
    case m of {
        Map list ->
            case list of {
                E -> True,
                L _ _ _ -> False
            }
    }

singleton : Int -> Int -> Map
singleton k v = Map (L k v E)

insert : Int -> Int -> Map -> Map
insert k v m =
    case m of {
        Map list -> Map (insert' k v list)
    }

insert' : Int -> Int -> List -> List
insert' k v l =
    case l of {
        E -> L k v E,
        L x y rest -> 
            if k == x 
                then L k v rest
                else if x < k 
                        then L x y (insert' k v rest)
                        else L k v l
    }

isNull : Map -> Bool
isNull m = 
    case m of {
        Map list -> 
            case list of {
                E -> True,
                L _ _ _ -> False
            }
    }

size : Map -> Int
size m = case m of { Map list -> size' list }

size' : List -> Int
size' list = 
    case list of {
        E -> 0,
        L _ _ rest -> 1 + size' rest
    }

member : Int -> Map -> Bool
member k m = 
    case m of {
        Map list -> member' k list
    }

member' : Int -> List -> Bool
member' k list =
    case list of {
        E -> False,
        L x _ rest -> 
            if k == x 
                then True
                else if x < k
                        then member' k rest
                        else False
    }

data MaybeInt = None | Number Int

lookupMap : Int -> Map -> MaybeInt
lookupMap k m = case m of { Map list -> lookup' k list }

lookup' : Int -> List -> MaybeInt
lookup' k list =
    case list of {
        E -> None,
        L x y rest -> 
            if k == x 
                then Number y
                else if x < k
                        then lookup' k rest
                        else None
    }

delete : Int -> Map -> Map
delete k m = case m of { Map list -> Map (delete' k list) }

delete' : Int -> List -> List
delete' k list =
    case list of {
        E -> E,
        L x y rest -> 
            if k == x 
                then rest
                else if x < k
                        then L x y (delete' k rest)
                        else list
    }

unionWith : (Int -> Int -> Int) -> Map -> Map -> Map
unionWith f map1 map2 =
    case map1 of {
        Map list1 -> 
            case map2 of {
                Map list2 -> Map (unionWithList f list1 list2)
            }
    }

unionWithList : (Int -> Int -> Int) -> List -> List -> List
unionWithList f list1 list2 =
    case list1 of {
        E -> list2,
        L a b rest1 ->
            case list2 of {
                E -> list1,
                L c d rest2 ->
                    if a == c
                        then L a (f b d) (unionWithList f rest1 rest2)
                        else if a < c
                            then L a b (unionWithList f rest1 list2)
                            else L c d (unionWithList f list1 rest2)
            }
    }

fromList : List -> Map
fromList list = Map list

toList : Map -> List
toList map = case map of { Map list -> list}

main : Bool
main = 
    let m = insert 9 9 (insert 7 7 (insert 5 5 (insert 3 3 (singleton 1 1))))   in 
    let emptyNullB = (not (empty m)) && (not (isNull m))                        in
    let sizeB = 5 == size m                                                     in
    let memberB = (member 3 m) && (not (member 4 m))                            in
    let lookupB = 
        case lookupMap 3 m of {
            None -> False,
            Number x -> x == 3
        }                                                                       in
    let deleteB = 4 == size (delete 9 m)                                        in
    let f = (\x : Int -> (\y : Int -> x + y))                                   in
    let unionB = 7 == size (unionWith f m (insert 2 2 (insert 4 4 m)))          in
    let fromB = size m == size (fromList (toList m))                            in
    emptyNullB && sizeB && memberB && lookupB && deleteB && unionB && fromB
--result = True
