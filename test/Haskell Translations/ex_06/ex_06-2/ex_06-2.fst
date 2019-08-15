-- VI exercise 2

data Map = M List
data List = End | List Int Int List

empty : Map
empty = M End

singleton : Int -> Int-> Map
singleton k v = M (List k v End)

insertMap : Int -> Int -> Map -> Map
insertMap k v map' = case map' of { M list -> M (insertList k v list) }

insertList : Int -> Int -> List -> List 
insertList k v list =
    case list of {
        End -> List k v End,
        List x y rest -> 
            if k == x 
                then List k v rest
                else if k < x
                        then List k v list
                        else List x y (insertList k v rest)
    }

isNull : Map -> Bool
isNull map' =
    case map' of {
        M list -> case list of {
                        End -> True,
                        List _ _ _ -> False
                    }
    }

size : Map -> Int
size map' = 
    case map' of { M list -> sizeList list }

sizeList : List -> Int
sizeList list = 
    case list of {
        End -> 0,
        List _ _ rest -> 1 + sizeList rest
    }

member : Int -> Map -> Bool
member k map' = case map' of { M list -> memberList k list}

memberList : Int -> List -> Bool
memberList k list =
    case list of {
        End -> False,
        List x _ rest -> 
            if k == x
                then True
                else if k < x
                        then False
                        else memberList k rest
    }

data MaybeInt = Empty | Number Int

lookupMap : Int -> Map -> MaybeInt
lookupMap k map' = case map' of { M list -> lookupList k list }

lookupList : Int -> List -> MaybeInt
lookupList k list =
    case list of {
        End -> Empty,
        List x y rest -> 
            if k == x 
                then Number y
                else if k < x 
                        then Empty
                        else lookupList k rest
    }

delete : Int -> Map -> Map
delete k map' = case map' of { M list -> M (deleteList k list) }

deleteList : Int -> List -> List
deleteList k list = 
    case list of {
        End -> End,
        List x y rest -> 
            if k == x
                then rest
                else if k < x 
                    then list
                    else List x y (deleteList k rest)
    }

unionWith : (Int -> Int -> Int) -> Map -> Map -> Map
unionWith f map1 map2 =
    case map1 of {
        M list1 -> 
            case map2 of {
                M list2 -> M (unionWithList f list1 list2)
            }
    }

unionWithList : (Int -> Int -> Int) -> List -> List -> List
unionWithList f list1 list2 =
    case list1 of {
        End -> list2,
        List a b rest1 ->
            case list2 of {
                End -> list1,
                List c d rest2 ->
                    if a == c
                        then List a (f b d) (unionWithList f rest1 rest2)
                        else if a < c
                            then List a b (unionWithList f rest1 list2)
                            else List c d (unionWithList f list1 rest2)
            }
    }

fromList : List -> Map
fromList list = M list

toList : Map -> List
toList map' = case map' of { M list -> list }

plus : Int -> Int -> Int
plus x y = x + y

main : Map
main = 
    let map' = M (List 1 1 (List 4 4 (List 9 9 (List 10 10 End)))) in

    let _ = empty                           in -- M End
    let _ = singleton 1 2                   in -- M (List 1 2 End)
    let _ = insertMap 0 0 map'              in -- M (List 0 0 (List 1 1 (List 4 4 (List 9 9 (List 10 10 End)))))
    let _ = insertMap 9 0 map'              in -- M (List 1 1 (List 4 4 (List 9 0 (List 10 10 End))))
    let _ = insertMap 8 8 map'              in -- M (List 1 1 (List 4 4 (List 8 8 (List 9 9 (List 10 10 End)))))
    let _ = insertMap 11 11 map'            in -- M (List 1 1 (List 4 4 (List 9 9 (List 10 10 (List 11 11 End)))))
    let _ = isNull empty                    in -- True
    let _ = isNull map'                     in -- False
    let _ = size empty                      in -- 0
    let _ = size map'                       in -- 4
    let _ = member 0 map'                   in -- False
    let _ = member 4 map'                   in -- True
    let _ = lookupMap 0 map'                in -- Empty
    let _ = lookupMap 9 map'                in -- Number 9
    let _ = delete 9 map'                   in -- M (List 1 1 (List 4 4 (List 10 10 End)))
    -- let f = (\x : Int -> (\y : Int -> x+y)) in
    let map'' = insertMap 5 5 (delete 9 map')                       in
    let _ = unionWith plus map' map'        in -- M (List 1 2 (List 4 8 (List 9 18 (List 10 20 End))))
    let _ = unionWith plus map' map''       in -- M (List 1 2 (List 4 8 (List 5 5 (List 9 9 (List 10 20 End)))))
    fromList (toList map')
--result = M (List 1 1 (List 4 4 (List 9 9 (List 10 10 End))))