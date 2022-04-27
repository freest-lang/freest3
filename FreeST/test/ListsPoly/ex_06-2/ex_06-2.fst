-- VI exercise 2

data Map = M [(Int,Int)] 

empty : Map
empty = M []

singleton : Int -> Int -> Map
singleton k v = M ([(k,v)])

insertMap : Int -> Int -> Map -> Map
insertMap k v map' = case map' of { M list -> M (insertList k v list) }

insertList : Int -> Int -> [(Int,Int)] -> [(Int,Int)] 
insertList k v list =
    case list of {
        [] -> [(k,v)],
        xy :: rest -> 
            let (x,y) = xy in
            if k == x 
                then (k,v) :: rest
                else if k < x
                        then (k,v) :: list
                        else (x,y) :: (insertList k v rest)
    }

isNull : Map -> Bool
isNull map' =
    case map' of {
        M list -> case list of {
                        []   -> True,
                        _::_ -> False
                    }
    }

size : Map -> Int
size map' = 
    case map' of { M list -> sizeList list }

sizeList : [(Int,Int)] -> Int
sizeList list = 
    case list of {
        [] -> 0,
        _ :: rest -> 1 + sizeList rest
    }

member : Int -> Map -> Bool
member k map' = case map' of { M list -> memberList k list}

memberList : Int -> [(Int,Int)] -> Bool
memberList k list =
    case list of {
        [] -> False,
        xy :: rest -> 
            let (x,y) = xy in
            if k == x
                then True
                else if k < x
                        then False
                        else memberList k rest
    }

data MaybeInt = Empty | Number Int

lookupMap : Int -> Map -> MaybeInt
lookupMap k map' = case map' of { M list -> lookupList k list }

lookupList : Int -> [(Int,Int)] -> MaybeInt
lookupList k list =
    case list of {
        [] -> Empty,
        xy :: rest -> 
            let (x,y) = xy in
            if k == x 
                then Number y
                else if k < x 
                        then Empty
                        else lookupList k rest
    }

delete : Int -> Map -> Map
delete k map' = case map' of { M list -> M (deleteList k list) }

deleteList : Int -> [(Int,Int)] -> [(Int,Int)] 
deleteList k list = 
    case list of {
        [] -> [],
        xy :: rest -> 
            let (x,y) = xy in
            if k == x
                then rest
                else if k < x 
                    then list
                    else (x,y) :: (deleteList k rest)
    }

unionWith : (Int -> Int -> Int) -> Map -> Map -> Map
unionWith f map1 map2 =
    case map1 of {
        M list1 -> 
            case map2 of {
                M list2 -> M (unionWithList f list1 list2)
            }
    }

unionWithList : (Int -> Int -> Int) -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] 
unionWithList f list1 list2 =
    case list1 of {
        [] -> list2,
        ab rest1 ->
            let (a,b) = ab in
            case list2 of {
                [] -> list1,
                cd :: rest2 ->
                    let (c,d) = cd in
                    if a == c
                        then (a,(f b d)) :: (unionWithList f rest1 rest2)
                        else if a < c
                            then (a,b) :: (unionWithList f rest1 list2)
                            else (c,d) :: (unionWithList f list1 rest2)
            }
    }

fromList : [(Int,Int)] -> Map
fromList list = M list

toList : Map -> [(Int,Int)] 
toList map' = case map' of { M list -> list }

plus : Int -> Int -> Int
plus x y = x + y

main : Map
main = 
    let map' = M [(1,1),(4,4),(9,9),(10,10)] in

    let _ = empty                           in -- M []
    let _ = singleton 1 2                   in -- M [(1,2)]
    let _ = insertMap 0 0 map'              in -- M [(0,0),(1,1),(4,4),(9,9),(10,10)]
    let _ = insertMap 9 0 map'              in -- M [(1,1),(4,4),(9,0),(10,10)]
    let _ = insertMap 8 8 map'              in -- M [(1,1),(4,4),(8,8),(9,9),(10,10)]
    let _ = insertMap 11 11 map'            in -- M [(1,1),(4,4),(9,9),(10,10),(11,11)]
    let _ = isNull empty                    in -- True
    let _ = isNull map'                     in -- False
    let _ = size empty                      in -- 0
    let _ = size map'                       in -- 4
    let _ = member 0 map'                   in -- False
    let _ = member 4 map'                   in -- True
    let _ = lookupMap 0 map'                in -- Empty
    let _ = lookupMap 9 map'                in -- Number 9
    let _ = delete 9 map'                   in -- M [(1,1),(4,4),(10,10)]
    -- let f = (\x : Int -> (\y : Int -> x+y)) in
    let map'' = insertMap 5 5 (delete 9 map')  in
    let _ = unionWith plus map' map'        in -- M [(1,2),(4,8),(9,18),(10,20)]
    let _ = unionWith plus map' map''       in -- M [(1,2),(4,8),(5,5),(9,9),(10,20)]
    fromList (toList map')
--result = M [(1,1),(4,4),(9,9),(10,10)]