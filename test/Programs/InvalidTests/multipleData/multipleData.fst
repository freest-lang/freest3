
data Tree = Node Int Tree Tree | Leaf
data Tree = Node Int Tree Tree | Leaf

-- It doesn't really matter, it should not throw an error here
main : Int
main = 10

-- It should have 2 params but it only has one
fun : Int -> Bool
fun x = True
