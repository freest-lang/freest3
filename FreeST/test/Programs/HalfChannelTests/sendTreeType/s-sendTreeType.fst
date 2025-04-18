{- |
Module      :  Exchange a binary tree on a channel
Description :  As in "Context-Free Session Types", ICFP'16
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

data Tree = Leaf | Node Int Tree Tree

type TreeChannel = &{
  LeafC: Skip,
  NodeC: ?Int ; TreeChannel ; TreeChannel
 }

read : TreeChannel; a -> (Tree, a)
read c =
  match c with {
    LeafC c ->
      (Leaf, c),
    NodeC c ->
      let (x, c) = receive c in
      let (left, c) = read  @(TreeChannel ; a) c in
      let (right, c) = read  @a c in
      (Node x left right, c)
  }

main : Tree
main =
  let (tree, c) = newHcServer @(TreeChannel ; Wait) ("127.0.0.1", "8081") |>
  read  @Wait in 
  wait c;
  tree

