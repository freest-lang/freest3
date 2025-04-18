-- Represents a n-Tree structure where each node has 0..n children.
data Tree = Empty | Node Int TreeList

-- List of Trees
data TreeList = Nil | Cons Tree TreeList

type TreeChannel = +{
  Node : !Int; TreeListChannel,
  Empty: Skip }

type TreeListChannel = +{
  Cons: TreeChannel; TreeListChannel,
  Nil : Skip }


-- ===== SENDING =====

sendTree : Tree -> TreeChannel;a -> a
sendTree tree c =
  case tree of {
    Empty ->
      select Empty c,
    Node i children ->
      sendTreeList @a children $ send i $ select Node c
  }

and sendTreeList : TreeList -> TreeListChannel;a -> a
sendTreeList list c =
  case list of {
    Nil ->
      select Nil c,
    Cons tree rest ->
      sendTreeList @a rest $ sendTree @(TreeListChannel ; a) tree $ select Cons c
  }

-- ===== MAIN =====

-- This definition represents the following Tree:
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

clientSendTree : TreeChannel;Close -> ()
clientSendTree c =
  sendTree @Close aTree c
  |> close

main : ()
main =
  newHcClient @(TreeChannel;Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  clientSendTree