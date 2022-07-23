data Tree = Leaf | Node Tree Int Tree
type WTree:1S = +{Leaf: Skip, Node: !RTree ; !Int ; !RTree};End
type RTree:1S = &{Leaf: Skip, Node: ?RTree ; ?Int ; ?RTree};End

sendTree : Tree -> WTree -> ()
sendTree t c =
  case t of {
    Leaf -> select Leaf c & close,
    Node t1 x t2 ->
      let c = select Node c in
      -- left
      let (w1, r1) = new WTree in
      let c = send r1 c in
      sendTree t1 w1;
      -- root
      let c = send x c in
      -- right
      let (w2, r2) = new WTree in
      let c = send r2 c in
      close c;
      sendTree t2 w2
  }

receiveTree : RTree -> Tree
receiveTree c =
  match c with {
    Leaf c -> close c; Leaf,
    Node c ->
      -- left
      let (r1, c) = receive c in
      let t1 = receiveTree r1 in
      -- root
      let (x, c) = receive c in
      -- right
      let (r2, c) = receive c in
      let t2 = receiveTree r2 in
      close c;
      Node t1 x t2
  }

singleton : Int -> Tree
singleton x = Node Leaf x Leaf

aTree : Tree
aTree =
  Node (
    Node 
      (singleton 4)
      6
      (singleton 8))
    0
    (singleton 5)

main : Tree
main =
  let (w, r) = new WTree;End in
  sendTree aTree w; 
  receiveTree r
