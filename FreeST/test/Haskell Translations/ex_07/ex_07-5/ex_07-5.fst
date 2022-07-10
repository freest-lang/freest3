-- VII exercise 5

data Tree = Leaf | Node Int Tree Tree
data List = End | List Int List

insert : Int -> Tree -> Tree
insert x tree =
    case tree of {
        Leaf -> Node x Leaf Leaf,
        Node y t1 t2 ->
            if x == y
                then Node y t1 t2
                else 
                    if x < y
                        then Node y (insert x t1) t2
                        else Node y t1 (insert x t2)
    }

isEmpty : Tree -> Bool
isEmpty tree =
    case tree of {
        Leaf -> True,
        Node _ _ _ -> False
    }

size : Tree -> Int
size tree =
    case tree of {
        Leaf -> 0,
        Node _ t1 t2 -> 1 + size t1 + size t2
    }

depth : Tree -> Int
depth tree =
    case tree of {
        Leaf -> 0,
        Node _ t1 t2 -> 1 + (\x:Int -> (\y:Int -> if x<y then y else x)) (size t1) (size t2) 
    }

flatten : Tree -> List
flatten tree =
    case tree of {
        Leaf -> End,
        Node x t1 t2 -> union (union (flatten t1) (List x End)) (flatten t2)
    }

union : List -> List -> List
union list1 list2 =
    case list1 of {
        End -> list2,
        List x rest1 -> List x (union rest1 list2)
    }

isPerfect : Tree -> Bool
isPerfect tree = 
    case tree of {
        Leaf -> True,
        Node _ t1 t2 -> isPerfect t1 && isPerfect t2 && size t1 == size t2 
    }

invert : Tree -> Tree
invert t =
    case t of {
        Leaf -> Leaf,
        Node x t1 t2 -> Node x (invert t2) (invert t1)
    }

-- simplificated
makeTree : List -> Tree
makeTree list =
    case list of {
        End -> Leaf,
        List x rest -> insert x (makeTree rest)
    }

isIn : Int -> Tree -> Bool
isIn x tree =
    case tree of {
        Leaf -> False,
        Node y t1 t2 -> 
            if x == y
                then True
                else 
                    if x < y
                        then isIn x t1
                        else isIn x t2
    }

allIn : List -> Tree -> Bool
allIn list tree = 
    case list of {
        End -> True,
        List x rest -> isIn x tree && (allIn rest tree)
    }

listSize : List -> Int
listSize list =
    case list of {
        End -> 0,
        List _ rest -> 1 + listSize rest
    }

main : Bool
main = 
    let t = insert 8 (insert 6 (insert 2 (insert 4 Leaf)))  in
    let emptyB = not (isEmpty t)                            in
    let depthB = 3 == depth t                               in
    let flatB = 4 == listSize (flatten t)                   in
    let perfectB = not (isPerfect t)                        in
    let invertB = 4 == size (invert t)                      in
    let list = List 1 (List 2 (List 3 (List 4 End)))        in
    let makerB = listSize list == size (makeTree list)      in
    let isInB = isIn 4 t && not (isIn 3 t)                  in
    let allInB1 = not (allIn list t)                        in
    let allInB2 = allIn list (insert 1 (insert 3 t))        in
    emptyB && depthB && flatB && perfectB && invertB &&
    makerB && isInB && allInB1 && allInB2
--result = True
