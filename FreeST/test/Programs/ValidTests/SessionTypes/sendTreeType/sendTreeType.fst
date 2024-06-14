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

read : dualof TreeChannel; a -> (Tree, a)
read c =
  match c with {
    LeafC c ->
      (Leaf, c),
    NodeC c ->
      let (x, c) = receive c in
      let (left, c) = read  @(dualof TreeChannel ; a) c in
      let (right, c) = read  @a c in
      (Node x left right, c)
  }

aTree, main : Tree

aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main =
  let (writer, reader) = new @(TreeChannel;Close) () in
  fork  @() (\_:()1-> write  @Close aTree writer |> close);
  let (tree, reader) = read  @Wait reader in 
  wait reader;
  tree

