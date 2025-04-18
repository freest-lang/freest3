-- {- |
-- Module      :  Exchange a binary tree on a channel
-- Description :  As in "Context-Free Session Types", ICFP'16
-- Copyright   :  (c) LASIGE and University of Lisbon, Portugal
-- Maintainer  :  balmeida@lasige.di.fc.ul.pt
-- -}

data Tree = Leaf | Node Tree Int Tree

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

main : Tree
main =
  newHcServer @TreeChannel ("127.0.0.1", "8081") |>
  readTree
