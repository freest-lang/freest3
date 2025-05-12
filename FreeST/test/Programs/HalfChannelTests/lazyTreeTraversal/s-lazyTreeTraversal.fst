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


type XploreTreeChan = &{LeafC: Skip,
                       NodeC: XploreNodeChan}

type XploreNodeChan = +{
   Value  : ?Int;XploreNodeChan ,
   Left   : XploreTreeChan ; XploreNodeChan ,
   Right  : XploreTreeChan ; XploreNodeChan ,
   Exit   : Skip
 }

-- The server. Compute the product of the values in a tree;
-- explicitely request the values; stop as soon a zero is received
server : XploreTreeChan ;a -> Int 1-> (a, Int)
server c1 n =
  match c1 with {
    LeafC c1 ->
      (c1, n),
    NodeC c1 ->
      serverNode @a c1 n
  }

and serverNode : XploreNodeChan;a -> Int 1-> (a, Int)
serverNode c n =
  let (m, c) = receive (select Value c) in
  if m == 0
  then 
    (select Exit c, 0)
  else
    let c = select Left c in
    let (c, m) = server @(XploreNodeChan ; a) c (m * n) in
    let (c, k) = server @(XploreNodeChan ; a) (select Right c) m in
    (select Exit c, k)

main : Int
main =
  let c = newHcServer @(XploreTreeChan; Wait) ("127.0.0.1", "8081") in
  let (c1, n) = server @Wait c 1 in 
  wait c1;
  n
