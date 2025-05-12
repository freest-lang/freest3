{- |
Module      :  LazyTreeTraversal
Description :  Computes the product of the values in a tree
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos

The example is from Thiemann and Vasconcelos: "Context-Free Session
Types" (listing 4)

The server computes the product of the values in a tree; explicitly
request the values; stop as soon a zero is received.

The client sends the tree as requested by the server.

The type describing this interaction is mutually recursive. The type
described by XploreNodeChan is tail-recursive like a regular session
type, but XploreTreeChan is not since its invocations are intertwined
with XploreNodeChan (adapted from the paper).

-}

data Tree = Leaf | Node Int Tree Tree

type XploreTreeChan = +{LeafC: Skip,
                       NodeC: XploreNodeChan}

type XploreNodeChan = &{
   Value : !Int;XploreNodeChan ,
   Left : XploreTreeChan ; XploreNodeChan ,
   Right : XploreTreeChan ; XploreNodeChan ,
   Exit : Skip
 }

-- The client. Send the tree as requested by the server.
exploreTree : XploreTreeChan;a -> Tree 1-> a
exploreTree c tree =
  case tree of {
    Leaf ->
      select LeafC c,
    Node x l r ->
      exploreNode @a (select NodeC c) x l r
    }

and exploreNode : XploreNodeChan;a -> Int 1-> Tree 1-> Tree 1-> a
exploreNode c x l r =
  match c with {
    Value c ->
      exploreNode @a (send x c) x l r,
    Left c ->
      let c = exploreTree @(XploreNodeChan ; a) c l in
      exploreNode @a c x l r,
    Right c ->
      let c = exploreTree @(XploreNodeChan ; a) c r in
      exploreNode @a c x l r,
    Exit c ->
      c
  }

aTree : Tree
aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main : ()
main =
  let c = newHcClient1 @(XploreTreeChan;Close) ("127.0.0.1", "8081") in
  exploreTree @Close c aTree |> close
