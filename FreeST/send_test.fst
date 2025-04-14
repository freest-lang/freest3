data Tree = Leaf | Node Tree Int Tree

aTree : Tree
-- aTree = Node Leaf 5 Leaf
-- aTree = Node Leaf 7 (Node Leaf 5 Leaf)
aTree = Node (Node Leaf 5 Leaf) 7 (Node (Node Leaf 11 Leaf) 9 (Node Leaf 15 Leaf))
-- aTree =
--   Node (
--     Node 
--       (singleton 4)
--       6
--       (singleton 8))
--     0
--     (singleton 5)

type TreeChannel = TreeC ; Wait

type TreeC = &{
  LeafC: Skip,
  NodeC: TreeC ; ?Int ; TreeC
 }

write : Tree -> dualof TreeC ; a -> a
write Leaf c = select LeafC c
write (Node l x r) c = 
  c |> select NodeC
    |> write @(!Int;dualof TreeC;a) l
    |> send x
    |> write @a r

writeTree : Tree -> dualof TreeChannel -> ()
writeTree tree writer =
  write @Close tree writer |> close

main : ()
main =
  let c = newHcClient @(dualof TreeChannel) (("127.0.0.1", "8080"), "127.0.0.1:8081") in
  writeTree aTree c;
  ()



-- singleton : Int -> Tree
-- singleton x = Node Leaf x Leaf

-- main : ()
-- main =
--   let c = newHcClient1 @(!Tree;Close) ("127.0.0.1", "8081") in
--   send aTree c |> close ;
--   ()