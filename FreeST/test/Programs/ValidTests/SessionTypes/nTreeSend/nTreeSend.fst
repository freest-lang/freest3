main : Tree
main =
  let (client, server) = new TreeChannel;End in
  fork @() (\_:()1-> clientSendTree client);
  let (t, server) = receiveTree @End server in
  close server;
  t


clientSendTree : TreeChannel;End -> ()
clientSendTree c =
  sendTree @End aTree c
  & close

-- This function represents the following Tree:
--                     0
--           1         2       3
--    7      8    6           4 5
--   13    11 20
aTree : Tree
aTree = Node 0 $ Cons (Node 1 $ Cons (Node 7 $ Cons (Node 13 Nil)
                                               Nil) $
                                Cons (Node 8 $ Cons (Node 11 Nil) $
                                               Cons (Node 20 Nil)
                                               Nil) $
                                Cons (Node 6 Nil)
                                Nil) $
                 Cons (Node 2 Nil) $
                 Cons (Node 3 $ Cons (Node 4 Nil) $
                                Cons (Node 5 Nil)
                                Nil)
                 Nil


-- Represents a n-Tree structure where each node has 0..n children.
data Tree = Empty | Node Int TreeList

-- List of Trees
data TreeList = Nil | Cons Tree TreeList

type TreeChannel : 1S = +{
  Node : !Int; TreeListChannel,
  Empty: Skip }

type TreeListChannel : 1S = +{
  Cons: TreeChannel; TreeListChannel,
  Nil : Skip }


-- ===== SENDING =====
sendTree : forall a: 1S . Tree -> TreeChannel;a -> a
sendTree tree c =
  case tree of {
    Empty ->
      select Empty c,
    Node i children ->
      sendTreeList @a children $ send i $ select Node c
  }

sendTreeList : forall a: 1S . TreeList -> TreeListChannel;a -> a
sendTreeList list c =
  case list of {
    Nil ->
      select Nil c,
    Cons tree rest ->
      sendTreeList @a rest $ sendTree @(TreeListChannel ; a) tree $ select Cons c
  }

-- ===== RECEIVING =====
receiveTree : forall a: 1S . dualof TreeChannel;a -> (Tree, a)
receiveTree c =
  match c with {
    Empty c ->
      (Empty, c),
    Node c ->
      let (i, c)        = receive c in
      let (children, c) = receiveTreeList @a c in
      (Node i children, c)
  }

receiveTreeList : forall a: 1S . dualof TreeListChannel;a -> (TreeList, a)
receiveTreeList c =
  match c with {
    Nil c ->
      (Nil, c),
    Cons c ->
      let (tree, c) = receiveTree @(dualof TreeListChannel ; a) c in
      let (rest, c) = receiveTreeList @a c in
      (Cons tree rest, c)
  }
