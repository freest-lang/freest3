-- {- |
-- Module      :  Exchange a binary tree on a channel
-- Description :  As in "Context-Free Session Types", ICFP'16
-- Copyright   :  (c) LASIGE and University of Lisbon, Portugal
-- Maintainer  :  balmeida@lasige.di.fc.ul.pt
-- -}

data Tree = Leaf | Node Tree Int Tree

aTree : Tree
aTree = Node (Node Leaf 5 Leaf) 7 (Node (Node Leaf 11 Leaf) 9 (Node Leaf 15 Leaf))

type TreeChannel = TreeC ; Close

type TreeC = +{
  LeafC: Skip,
  NodeC: TreeC ; !Int ; TreeC
 }

write : Tree -> TreeC ; a -> a
write Leaf c = select LeafC c
write (Node l x r) c = 
  c |> select NodeC
    |> write @(!Int; TreeC;a) l
    |> send x
    |> write @a r

writeTree : Tree -> TreeChannel -> ()
writeTree tree writer =
  write @Close tree writer |> close

main : ()
main =
  newHcClient @TreeChannel (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  writeTree aTree