-- ==== MAIN ====
-- This main acts as the server that receives the Tree sent by a client
main : Tree
main =
  let (w, r) = new TreeC in
  fork (\_:() 1-> treeClient w);
  --fork@() $ badClientPrematureEnd w;
  --fork@() $ badClientSendExtraValue w;
  --fork@() $ badClientSendExtraLeaf w;
  --fork@() $ badClientForgotRight w;
  --fork@() $ badClientSendOnlyValue w;
  receiveTree r


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
stackPush : Tree -> TreeStack -> TreeStack
stackPush = Value

-- Pop the top Tree from the Stack (Leaf if empty)
stackPop : TreeStack -> (TreeStack, Tree)
stackPop Empty        = (Empty, Leaf)
stackPop (Value t ts) = (ts, t)

-- Is the Stack empty
stackIsEmpty : TreeStack -> Bool
stackIsEmpty Empty       = True
stackIsEmpty (Value _ _) = False

-- Size of the Stack
stackSize : TreeStack -> Int
stackSize Empty        = 0
stackSize (Value _ ts) = 1 + stackSize ts

-- Channel to send/receive a Tree. It is important that both sender and receiver
--  agree on an order to traverse the Tree.
--  (In our particular case we will use PREORDER - node, left, right)
type TreeC : 1S = +{
  ValueC: !Int; TreeC,
  LeafC : TreeC,
  EndC   : Skip }


-- Sends a tree through a TreeC
sendTree : Tree -> TreeC -> TreeC
sendTree Leaf           c = select LeafC c
sendTree (Node i lt rt) c = send i $ select ValueC $ sendTree lt $ sendTree rt c

-- Facade function to receive a Tree through a channel
receiveTree : dualof TreeC -> Tree
receiveTree = receiveTree_ Empty

-- Receives a Tree from a TreeC
--  This function also serves as an abstraction to the TreeStack usage
receiveTree_ : TreeStack -> dualof TreeC -> Tree
receiveTree_ ts (ValueC c) =
      let (i, c)   = receive c in
      errorWhen (stackIsEmpty ts) "Received Value without receiveing left AND right subtrees";
      let (ts, lt) = stackPop ts in
      errorWhen (stackIsEmpty ts) "Received Value without receiveing left OR right subtrees";
      let (ts, rt) = stackPop ts in
      let ts       = stackPush (Node i lt rt) ts in
      receiveTree_ ts c
receiveTree_ ts (LeafC c) =
      receiveTree_ (stackPush Leaf ts) c
receiveTree_ ts (EndC  c) =
      errorWhen (stackIsEmpty ts)  "Channel was closed without sending a Tree";
      errorWhen (stackSize ts > 1) "Channel was closed mid-stream or with leftover tree elements";
      snd@TreeStack@Tree $ stackPop ts

-- Generates an error with a given message if a given boolean is true
errorWhen : Bool -> String -> ()
errorWhen b s =
  if b
  then error@() s
  else ()

-- Simple treeClient that sends a Tree through a TreeC
treeClient : TreeC -> ()
treeClient c =
  let _ = select EndC $ sendTree aTree c in
  ()


-- ==== BAD CLIENTS ===
-- These are bad client implementations to test error situations

-- This bad client ends prematurely
badClientPrematureEnd : TreeC -> ()
badClientPrematureEnd c =
  let _ = select EndC c in
  ()

-- This bad client send an extra Value -1
badClientSendExtraValue : TreeC -> ()
badClientSendExtraValue c =
  let _ = select EndC $ send (-1) $ select ValueC $ sendTree aTree c in
  -- Bad Code         ===========================
  ()

-- This bad client send an extra Leaf
badClientSendExtraLeaf : TreeC -> ()
badClientSendExtraLeaf c =
  let _ = select EndC $ select LeafC $ sendTree aTree c in
  -- Bad  Code         =============
  ()

-- This client does not send the right subtree
badClientForgotRight: TreeC -> ()
badClientForgotRight c =
  let _ = select EndC $ badSendTree aTree c in
  -- Bad Code          ===========
  ()

-- This client only sends a value without sending leafs
badClientSendOnlyValue : TreeC -> ()
badClientSendOnlyValue c =
  let _  = select EndC $ send 1 $ select ValueC c in
  -- Bad code          ========================
  ()


-- Sends a tree through a TreeC
-- !!! But forgets to send right subtree
badSendTree : Tree -> TreeC -> TreeC
badSendTree Leaf           c = select LeafC c
badSendTree (Node i lt rt) c = send i $ select ValueC $ badSendTree lt c -- $ badSendTree rt c
