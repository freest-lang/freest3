data Tree = Leaf | Node Tree Int Tree

aTree : Tree
-- aTree = Node (Node Leaf 5 Leaf) 7 (Node (Node Leaf 11 Leaf) 9 (Node Leaf 15 Leaf))
aTree = Node Leaf 5 Leaf

type TreeChannel = TreeC ; Wait

type TreeC = &{
  LeafC: Skip,
  NodeC: TreeC ; ?Int ; TreeC
 }

read : TreeC ; a -> (Tree, a)
read (LeafC c) = (Leaf, c)
read (NodeC c) =
  let (l, c) = read @(?Int ; TreeC ; a) c in
  let (x, c) = receive c in
  let (r, c) = read @a c in
  (Node l x r, c)

readTree : TreeChannel -> Tree
readTree r = 
  let (tree, r) = read @Wait r in 
  wait r;
  tree

main : Bool
main =
  let c = newHcServer @(?Bool; Wait) ("127.0.0.1", "8081") in
  let (t, c1) = receive c in
  wait c1;
  t

-- main : Tree
-- main =
--     let c = newHcServer @(?Tree;Wait) ("127.0.0.1", "8081") in
--     let (t,c) = receive c in 
--     wait c;
--     t