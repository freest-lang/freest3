-- VII exercise 7

data Tree = Leaf | Node Int Tree Tree
data List = End | List Int List

insert : Int -> Tree -> Tree
insert v tree = 
    case tree of {
        Leaf -> Node v Leaf Leaf,
        Node x t1 t2 -> 
            if v == x 
                then tree
                else 
                    if x < v 
                        then Node x t1 (insert v t2)
                        else Node x (insert v t1) t2
    }

toList : Tree -> List
toList tree =
    case tree of {
        Leaf -> End,
        Node x t1 t2 -> union (union (toList t1) (List x End)) (toList t2)
    }

union : List -> List -> List
union list1 list2 =
    case list1 of {
        End -> list2,
        List x rest1 -> List x (union rest1 list2)
    }

equals : Tree -> Tree -> Bool
equals t1 t2 = equals' (toList t1) (toList t2)

equals' : List -> List -> Bool
equals' l1 l2 =
    case l1 of {
        End -> 
            case l2 of {
                End -> True,
                List _ _ -> False
            },
        List x rest1 ->
            case l2 of {
                End -> False,
                List y rest2 -> 
                    if x == y
                        then equals' rest1 rest2
                        else False
            }
    }

main : Bool
main = 
    let t1 = insert 1 (insert 2 (insert 0 (insert 9 (insert 6 Leaf)))) in
    let t2 = insert 3 t1 in
    equals t1 t1 && not (equals t1 t2)
--result = True
