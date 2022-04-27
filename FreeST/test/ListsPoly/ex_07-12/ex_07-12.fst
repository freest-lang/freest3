-- VII exercise 12

data Map = Map  [(Int,Int)]

insertMap : Int -> Int -> Map -> Map
insertMap k v m = case m of { Map list -> Map (insertList k v list) }

insertList : Int -> Int ->  [(Int,Int)] ->  [(Int,Int)]
insertList k v list =
    case list of {
        [] -> [(k,v)],
        xy :: rest ->
            let (x,y) = xy in
            if k == x 
                then (k,v) :: rest
                else 
                    if k < x 
                        then (k,v) :: list
                        else (x,y) :: (insertList k v rest)
    }

equals : Map -> Map -> Bool
equals m1 m2 = case m1 of { Map l1 -> case m2 of { Map l2 -> equals' l1 l2 } }

equals' :  [(Int,Int)] ->  [(Int,Int)] -> Bool
equals' l1 l2 =
    case l1 of {
        [] -> 
            case l2 of {
                [] -> True,
                _ :: _ -> False
            },
        kv :: r1 ->
            let (k,v) = kv in
            case l2 of {
                [] -> False,
                xy :: r2 ->
                    let (x,y) = xy in
                    if k == x && v == y
                        then equals' r1 r2
                        else False
            }
    }

main : Bool
main = 
    let map = insertMap 1 1 (insertMap 2 2 (insertMap 8 8 (insertMap 4 4 (insertMap 3 3 (Map ([])))))) in
    let b1 = equals map map                             in
    let b2 = not (equals map (insertMap 10 10 map))     in
    b1 && b2
--result = True