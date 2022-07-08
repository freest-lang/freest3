-- VII exercise 9

data Tree = Leaf | Node Int Tree Tree

fmap' : (Int -> Int) -> Tree -> Tree
fmap' f tree =
    case tree of {
        Leaf -> Leaf,
        Node x t1 t2 -> Node (f x) (fmap' f t1) (fmap' f t2)
    }

main : Tree
main = 
    let t = Node 2 (Node 1 (Node 0 Leaf Leaf) Leaf)(Node 4 (Node 3 Leaf Leaf) (Node 5 Leaf Leaf)) in
    let f = (\x : Int -> x*x) in
    fmap' f t
--result = Node 4 (Node 1 (Node 0 Leaf Leaf) Leaf) (Node 16 (Node 9 Leaf Leaf) (Node 25 Leaf Leaf))
