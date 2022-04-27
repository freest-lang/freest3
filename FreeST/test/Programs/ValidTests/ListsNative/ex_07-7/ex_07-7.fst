-- VII exercise 7

data Tree = Leaf | Node Int Tree Tree

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

toInt : Tree -> [Int]
toInt tree =
    case tree of {
        Leaf -> [],
        Node x t1 t2 -> union (union (toInt t1) ([x])) (toInt t2)
    }

union : [Int] -> [Int] -> [Int]
union list1 list2 =
    case list1 of {
        [] -> list2,
        x :: rest1 -> x :: (union rest1 list2)
    }

equals : Tree -> Tree -> Bool
equals t1 t2 = equals' (toInt t1) (toInt t2)

equals' : [Int] -> [Int] -> Bool
equals' l1 l2 =
    case l1 of {
        [] -> 
            case l2 of {
                [] -> True,
                _ :: _ -> False
            },
        x :: rest1 ->
            case l2 of {
                [] -> False,
                y :: rest2 -> 
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