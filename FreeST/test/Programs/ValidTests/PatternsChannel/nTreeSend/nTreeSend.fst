main : Tree
main =
  let (client, server) = new TreeChannel in
  fork (\_:() 1-> clientSendTree client);
  fst@Tree@Skip $ receiveTree@Skip server


clientSendTree : TreeChannel -> ()
clientSendTree c =
  let _ = sendTree@Skip aTree c in
  ()

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
  NodeC : !Int; TreeListChannel,
  EmptyC: Skip }

type TreeListChannel : 1S = +{
  ConsC: TreeChannel; TreeListChannel,
  NilC : Skip }


-- ===== SENDING =====
sendTree : forall a:1S . Tree -> TreeChannel;a -> a
sendTree Empty             c = select EmptyC c
sendTree (Node i children) c = sendTreeList@a children $ send i $ select NodeC c

sendTreeList : forall a:1S . TreeList -> TreeListChannel;a -> a
sendTreeList Nil              c = select NilC c
sendTreeList (Cons tree rest) c = sendTreeList@a rest 
                                $ sendTree@(TreeListChannel;a) tree $ select ConsC c

-- ===== RECEIVING =====
receiveTree : forall a:1S . dualof TreeChannel;a -> (Tree, a)
receiveTree (EmptyC c) = (Empty,c)
receiveTree (NodeC  c) = 
      let (i, c)        = receive c in
      let (children, c) = receiveTreeList@a c in
      (Node i children, c)

receiveTreeList : forall a:1S . dualof TreeListChannel;a -> (TreeList, a)
receiveTreeList (NilC  c) = (Nil, c)
receiveTreeList (ConsC c) = 
      let (tree, c) = receiveTree@(dualof TreeListChannel;a) c in
      let (rest, c) = receiveTreeList@a c in
      (Cons tree rest, c)
