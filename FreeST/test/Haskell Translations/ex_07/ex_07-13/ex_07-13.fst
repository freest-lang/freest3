-- VII exercise 13

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

data Showable = B Char Showable | Item Int Char Int Char Showable | E 

showMap : Map -> Showable
showMap map = case map of { Map list -> B '{' (showList' list) }

showList' : List -> Showable
showList' list = 
    case list of {
        End -> E,
        List x y rest ->
            case rest of {
                End -> Item x ':' y '}' E,
                List _ _ _ -> Item x ':' y ',' (showList' rest)
            }
    }

main : Showable
main = 
    let map = insertMap 1 1 (insertMap 2 2 (insertMap 8 8 (insertMap 4 4 (insertMap 3 3 (Map End))))) in
    showMap map
--result = B '{' (Item 1 ':' 1 ',' (Item 2 ':' 2 ',' (Item 3 ':' 3 ',' (Item 4 ':' 4 ',' (Item 8 ':' 8 '}' E)))))