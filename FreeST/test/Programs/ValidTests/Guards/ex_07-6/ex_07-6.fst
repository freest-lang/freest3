-- VII exercise 6

data Tree = Leaf | Node Int Tree Tree
data List = Nil | List Int List

tree_fold1 : (Int -> Bool -> Bool -> Bool) -> Bool -> Tree -> Bool
tree_fold1 f e Leaf           = e
tree_fold1 f e (Node x t1 t2) = f x (tree_fold1 f e t1) (tree_fold1 f e t2)

tree_fold2 : (Int -> Int -> Int -> Int) -> Int -> Tree -> Int
tree_fold2 f e Leaf           = e
tree_fold2 f e (Node x t1 t2) = f x (tree_fold2 f e t1) (tree_fold2 f e t2)

tree_fold3 : (Int -> List -> List -> List) -> List -> Tree -> List
tree_fold3 f e Leaf           = e
tree_fold3 f e (Node x t1 t2) = f x (tree_fold3 f e t1) (tree_fold3 f e t2)

tree_fold4 : (Int -> Tree -> Tree -> Tree) -> Tree -> Tree -> Tree
tree_fold4 f e Leaf           = e
tree_fold4 f e (Node x t1 t2) = f x (tree_fold4 f e t1) (tree_fold4 f e t2)

empty : Tree -> Bool
empty tree = tree_fold1 empty' True tree

empty' : Int -> Bool -> Bool -> Bool
empty' _ _ _ = False

size : Tree -> Int
size tree = tree_fold2 size' 0 tree

size' : Int -> Int -> Int -> Int
size' _ y z = 1 + y + z

sum' : Int -> Int -> Int -> Int
sum' x y z = x + y + z

depth : Tree -> Int
depth tree =  tree_fold2 depth' 0 tree

depth' : Int -> Int -> Int -> Int
depth' _ y z 
    | y < z     = 1 + z 
    | otherwise = 1 + y

flatten : Tree -> List
flatten tree = tree_fold3 flatten' Nil tree

flatten' : Int -> List -> List -> List
flatten' x Nil l2           = List x l2
flatten' x (List y rest) l2 = List y (flatten' x rest l2)

invert : Tree -> Tree
invert tree = tree_fold4 invert' Leaf tree

invert' : Int -> Tree -> Tree -> Tree
invert' x t1 t2 = Node x t2 t1

isPerfect : Tree -> Bool
isPerfect Leaf = True
isPerfect (Node _ t1 t2) = isPerfect t1 
                        && isPerfect t2 
                        && (size t1) == (size t2)

isIn : Int -> Tree -> Bool
isIn x tree = tree_fold1 (isIn' x) False tree

isIn' : Int -> Int -> Bool -> Bool -> Bool
isIn' x y e1 e2 = x==y || e1 || e2

allIn : Tree -> Tree -> Bool
allIn Leaf             t2 = True
allIn (Node x t11 t12) t2 = (isIn x t2) 
                         && (allIn t11 t2) 
                         && (allIn t12 t2)

main : Bool
main = 
    let t = Node 2 (Node 1 Leaf Leaf) (Node 4 (Node 3 Leaf Leaf) Leaf) in
    let emptyB = (not (empty t)) && (empty Leaf) in
    let sizeB = 4 == size t in
    let depthB = 3 == depth t && 0 == depth Leaf in
    let flattenB = 4 == listSize (flatten t) in
    let isPerB = not (isPerfect t) in
    let invertB = 4 == size (invert t) in
    let isInB = (isIn 4 t) && (not (isIn 6 t)) in
    let allInB = allIn t t in
    emptyB && sizeB && depthB && flattenB && 
    isPerB && invertB && isInB && allInB
--result = True

listSize : List -> Int
listSize Nil           = 0
listSize (List _ rest) = 1 + listSize rest
