-- VII exercise 12

data Map = Map List
data List = End | List Int Int List

insertMap : Int -> Int -> Map -> Map
insertMap k v m = case m of { Map list -> Map (insertList k v list) }

insertList : Int -> Int -> List -> List
insertList k v list =
    case list of {
        End -> List k v End,
        List x y rest ->
            if k == x 
                then List k v rest
                else 
                    if k < x 
                        then List k v list
                        else List x y (insertList k v rest)
    }

equals : Map -> Map -> Bool
equals m1 m2 = case m1 of { Map l1 -> case m2 of { Map l2 -> equals' l1 l2 } }

equals' : List -> List -> Bool
equals' l1 l2 =
    case l1 of {
        End -> 
            case l2 of {
                End -> True,
                List _ _ _ -> False
            },
        List k v r1 ->
            case l2 of {
                End -> False,
                List x y r2 ->
                    if k == x && v == y
                        then equals' r1 r2
                        else False
            }
    }

main : Bool
main = 
    let map = insertMap 1 1 (insertMap 2 2 (insertMap 8 8 (insertMap 4 4 (insertMap 3 3 (Map End))))) in
    let b1 = equals map map                             in
    let b2 = not (equals map (insertMap 10 10 map))     in
    b1 && b2
--result = True