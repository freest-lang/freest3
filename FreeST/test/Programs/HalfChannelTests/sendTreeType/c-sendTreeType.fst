{- |
Module      :  Exchange a binary tree on a channel
Description :  As in "Context-Free Session Types", ICFP'16
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

data Tree = Leaf | Node Int Tree Tree

type TreeChannel = +{
  LeafC: Skip,
  NodeC: !Int ; TreeChannel ; TreeChannel
 }

write : Tree -> TreeChannel; a -> a
write t c =
  case t of {
    Leaf ->
      select LeafC c,
    Node x l r ->
      select NodeC c
      |> send x
      |> write  @(TreeChannel ; a) l
      |> write @a r
  }
aTree : Tree

aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main : ()
main =
  newHcClient1 @(TreeChannel;Close) ("127.0.0.1", "8081") |>
  write @Close aTree |> close

