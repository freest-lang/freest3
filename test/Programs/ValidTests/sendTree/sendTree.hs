{- |
Module      :  Send a tree on a channel
Description :  As in "Context-Free Session Types"
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

data Tree = Leaf | Node Int Tree Tree

{-

type TreeChannel = +{
  LeafC : Skip,
  NodeC : !Int ; TreeChannel ; TreeChannel
}

type TreeChannel = rec x::Su. +{
  LeafC : Skip,
  NodeC : !Int ; x ; x
}
-}

data A = LeafA | NodeA Int A

sendOne :: A -> (rec x . +{LeafC : Skip, NodeC: !Int;x}) -> ()
sendOne t c =
  case t of
    LeafA ->      
      let x = select LeafC c in        
      ()
    NodeA x l ->
      let w1 = select NodeC c in
      let w2 = send x w1 in
      let w3 = sendOne l w2 in
--      let w4 = sendOne r w3 in
      ()

receiveOne :: (rec x.&{LeafC: Skip, NodeC: ?Int;x}) -> (A, A)
receiveOne c =
  match c with
    LeafC c1 ->
      (LeafA, LeafA)
    NodeC c1 ->
      let x, c2 = receive c1 in
      let left, c3 = receiveOne c2 in
--      let right, c4 = receiveTree[a] c3 in
      (NodeA x left, left)




-- TODO:
-- sendTree :: forall a :: SU => Tree -> (rec x . +{LeafC : Skip, NodeC: !Int; x;x} ; a) -> ()
-- sendTree t c =
--   case t of
--     Leaf ->
--       let x = select LeafC c in
--       ()
--     Node x l r ->
--        let w1 = select NodeC c in
--        let w2 = send x w1 in
--        let w3 = sendTree [rec x.+{LeafC: Skip, NodeC: !Int;x;x} ; Skip] l w2 in
--      --  let w4 = sendTree [Skip] r w2 in       
--       ()

-- Type-safe serialization of a binary tree
-- sendTree :: forall a :: SU => Tree -> (rec x::SU.+{LeafC: Skip, NodeC: !Int;x;x} ; a) -> a
-- --sendTree :: Tree -> rec x.+{LeafC: Skip, NodeC: !Int;x;x} ; a -> a
-- sendTree t c =
--   case t of
--     Leaf ->
--       select LeafC c
--     Node x l r ->
--       let c1 = select NodeC c in
--       let c2 = send x c1 in
--       let c3 = sendTree[rec x.+{LeafC: Skip, NodeC: !Int;x;x} ; a] l c2 in
--       let c4 = sendTree[a] r c3 in
--       c4
      

-- receiveTree :: forall a :: SU => rec x.&{LeafC: Skip, NodeC: ?Int;x;x} ; a -> (Tree, a)
-- receiveTree c =
--   case c of
--     LeafC c1 ->
--       (Leaf, c1)
--     NodeC c1 ->
--       let x, c2 = receive c1 in
--       let left, c3 = receiveTree[rec x.&{LeafC: Skip, NodeC: ?Int;x;x} ; a] c2 in
--       let right, c4 = receiveTree[a] c3 in
--       (Node x left right, c4)

-- start :: Tree
-- start =
--  let inTree = Node 7 (Node 5 (Leaf) (Leaf)) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf)) in
--  let writer,reader = new rec x.+{LeafC: Skip, NodeC: !Int;x;x} in
--  let w = fork (sendTree[Skip] inTree writer) in
--  let outTree, r = receiveTree[Skip] reader in
--  outTree

{-
TODO:
try the same constructors for the datatype and the session type

        rec x.+{Leaf: Skip, Node: !Int;x;x}
-}

-- start :: Int
-- start = 10


start :: A
start =
 let inTree = NodeA 7 LeafA in
  -- (NodeA 5 (LeafA) (LeafA)) in
  -- (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf)) in
 let writer,reader = new (rec x . +{LeafC: Skip, NodeC: !Int;x}) in
 let w = fork (sendOne inTree writer) in
 let outTree, r = receiveOne reader in
 outTree
