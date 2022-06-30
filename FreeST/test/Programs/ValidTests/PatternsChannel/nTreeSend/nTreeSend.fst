main : Tree
main =
  let (client, server) = new TreeChannel in
  fork[()] $ clientSendTree client;
  fst[Tree, Skip] $ receiveTree[Skip] server


clientSendTree : TreeChannel -> ()
clientSendTree c =
  let _ = sendTree[Skip] aTree c in
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

type TreeChannel : SL = +{
  Node : !Int; TreeListChannel,
  Empty: Skip }

type TreeListChannel : SL = +{
  Cons: TreeChannel; TreeListChannel,
  Nil : Skip }


-- ===== SENDING =====
sendTree : forall a:SL . Tree -> TreeChannel;a -> a
sendTree Empty             c = select Empty c
sendTree (Node i children) c = sendTreeList[a] children $ send i $ select Node c

sendTreeList : forall a:SL . TreeList -> TreeListChannel;a -> a
sendTreeList Nil              c = select Nil c
sendTreeList (Cons tree rest) c = sendTreeList[a] rest 
                                $ sendTree[TreeListChannel;a] tree $ select Cons c

-- ===== RECEIVING =====
receiveTree : forall a:SL . dualof TreeChannel;a -> (Tree, a)
receiveTree (Empty c) = (Empty,c)
receiveTree (Node  c) = 
      let (i, c)        = receive c in
      let (children, c) = receiveTreeList[a] c in
      (Node i children, c)

receiveTreeList : forall a:SL . dualof TreeListChannel;a -> (TreeList, a)
receiveTreeList (Nil  x) = (Nil, c)
receiveTreeList (Cons c) = 
      let (tree, c) = receiveTree[dualof TreeListChannel;a] c in
      let (rest, c) = receiveTreeList[a] c in
      (Cons tree rest, c)
