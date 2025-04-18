{- |
Module      :  TreeTransform
Description :  Serializes and tranforms a tree object on a channel
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos

The example is from Almeida, Mordido, and Vasconcelos, "FreeST:
Context-free Session Types in a Functional Language"

The example serializes a tree object on a channel. The aim is to
transform a tree by interacting with a remote server. The client
process streams a tree on a (single) channel. In addition, for each
node sent, an integer is received.  The server process reads a tree
from the other end of the channel and, for each node received, sends
back the sum of the integer values under (and including) that node.

-}

data Tree = Leaf | Node Int Tree Tree

type TreeC = &{LeafC: Skip, NodeC: ?Int;TreeC;TreeC;!Int}

{-
  Reads a tree from a given channel;
  writes back on the channel the sum of the elements in the tree;
  returns this sum.
-}
treeSum : TreeC ; a -> (Int, a)
treeSum c =
  match c with {
    LeafC c ->
     (0, c),
    NodeC c ->
      let (x, c) = receive c in
      let (l, c) = treeSum  @(TreeC ; !Int ; a) c in
      let (r, c) = treeSum  @(!Int ; a) c in
      let c = send (x + l + r) c in
      (x + l + r, c)
  }

main : Int
main =
  let (v, c) = newHcServer @(TreeC ; Close) ("127.0.0.1", "8081") |>
  treeSum @Close in
  close c;
  v
