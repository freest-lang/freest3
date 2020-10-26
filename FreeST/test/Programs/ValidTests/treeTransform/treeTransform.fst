-- Represents a classical binary tree (Node LeftTree RightTree)
data Tree = Leaf | Node Int Tree Tree

-- Example of the following Tree:
--               1
--        2            6
--    8      3            7
--         5   4
aTree : Tree
aTree = Node 1 (Node 2 (Node 8 Leaf Leaf) (Node 3 (Node 5 Leaf Leaf) (Node 4 Leaf Leaf))) (Node 6 Leaf (Node 7 Leaf Leaf))


-- Represents a Stack of Tree
data TreeStack = Empty | Value Tree TreeStack

-- Push a Tree to the stack
stackPush : TreeStack -> Tree -> TreeStack
stackPush ts t = Value t ts

-- Pop the top Tree from the Stack (Leaf if empty)
stackPop : TreeStack -> (TreeStack, Tree)
stackPop ts =
  case ts of {
    Empty      -> (ts, Leaf),
    Value t ts -> (ts, t)
  }

-- Is the Stack empty
stackIsEmpty : TreeStack -> Bool
stackIsEmpty ts =
  case ts of {
    Empty     -> True,
    Value _ _ -> False
  }

-- Size of the Stack
stackSize : TreeStack -> Int
stackSize ts =
  case ts of {
    Empty      -> 0,
    Value _ ts -> 1 + stackSize ts
  }


-- Channel to send/receive a Tree. It is important that both sender and receiver
--  agree on an order to traverse the Tree.
--  (In our particular case we will use PREORDER - node, left, right)
type TreeC : SL = +{ Value: !Int; TreeC, Leaf: Skip; TreeC, End: Skip }


-- Sends a tree through a TreeC
sendTree : TreeC -> Tree -> TreeC
sendTree c t =
  case t of {
    Leaf ->
      let c = select c Leaf in
      c,
    Node i lt rt ->
      let c = sendTree c rt in
      let c = sendTree c lt in
      let c = select c Value in
      let c = send c i in
      c
  }


-- Facade function to receive a Tree through a channel
receiveTree : dualof TreeC -> Tree
receiveTree c = receiveTree_ c Empty

-- Receives a Tree from a TreeC
--  This function also serves as an abstraction to the TreeStack usage
--
-- => ERROR DETECTION:
--  - 'W' - Sent Value without sending left and right subtrees
--  - 'V' - Sent Value without sending left or right subtree
--  - 'E' - Closed channel without sending a Tree
--  - 'F' - Closed channel mid-stream or with extra Values
receiveTree_ : dualof TreeC -> TreeStack -> Tree
receiveTree_ c ts =
  match c with {
    Value c ->
      let (i, c)   = receive c in
      let _ = if (stackIsEmpty ts) then printCharLn 'W' else () in
      let (ts, lt) = stackPop ts in
      let _ = if (stackIsEmpty ts) then printCharLn 'V' else () in
      let (ts, rt) = stackPop ts in
      let ts       = stackPush ts (Node i lt rt) in
      receiveTree_ c ts,

    Leaf c ->
      let ts = stackPush ts Leaf in
      receiveTree_ c ts,

    End  c ->
      let _ = if (stackIsEmpty ts) then printCharLn 'E' else () in
      let _ = if (stackSize ts > 1) then printCharLn 'F' else () in
      let (ts, t) = stackPop ts in
      t
  }

-- Simple treeClient that sends a Tree through a TreeC
treeClient : TreeC -> ()
treeClient c  =
  let c = sendTree c aTree in
  let c = select c End in
  ()


-- ==== MAIN ====
-- This main acts as the server that receives the Tree sent by a client
main : Tree
main =
  let (w, r) = new TreeC in
  let _      = fork $ badClientSendExtraValue w in
  let t      = receiveTree r in
  t



-- ==== BAD CLIENTS ===
-- These are bad client implementations to test error situations

-- This bad client ends prematurely
badClientPrematureEnd : TreeC -> ()
badClientPrematureEnd c =
  let _ = select c End in
  ()

-- This bad client send an extra Value -1
badClientSendExtraValue : TreeC -> ()
badClientSendExtraValue c =
  let c = sendTree c aTree in
  -- == Bad code ==
  let c = select c Value in
  let c = send c (-1) in
  -- == Bad code ==
  let _ = select c End in
  ()

-- This bad client send an extra Leaf
badClientSendExtraLeaf : TreeC -> ()
badClientSendExtraLeaf c =
  let c = sendTree c aTree in
  -- == Bad code ==
  let c = select c Leaf in
  -- == Bad code ==
  let _ = select c End in
  ()

-- This client does not send the right subtree
badClientForgotRight: TreeC -> ()
badClientForgotRight c =
  -- == Bad code ==
  let c = badSendTree c aTree in
  -- == Bad code ==
  let c = select c End in
  ()


-- Sends a tree through a TreeC
-- !!! But forgets to send right subtree
badSendTree : TreeC -> Tree -> TreeC
badSendTree c t =
  case t of {
    Leaf ->
      let c = select c Leaf in
      c,
    Node i lt rt ->
      --let c = badSendTree c rt in
      let c = badSendTree c lt in
      let c = select c Value in
      let c = send c i in
      c
  }
