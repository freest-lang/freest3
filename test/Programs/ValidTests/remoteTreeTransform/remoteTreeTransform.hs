
data Tree = Leaf | Node Int Tree Tree
-- (rec xFormChan . +{LeafC: Skip, NodeC: !Int;xFormChan;xFormChan;?Int})



transform :: forall x => Tree -> (rec xFormChan . +{LeafC: Skip, NodeC: !Int;xFormChan;xFormChan;?Int});x -> (Tree, x)
transform tree c =
  case tree of
    Leaf ->
      (Leaf, select LeafC c)

    Node x l r ->
      let c1 = select NodeC c in
      let c2 = send x c1 in
      let l1, c3 = transform[(rec xFormChan . +{LeafC: Skip, NodeC: !Int;xFormChan;xFormChan;?Int});x] l c2 in
      let r1, c4 = transform[?Int;x] r c3 in
      -- let r1, c4 = transform[rec xFormChan . +{LeafC: Skip, NodeC: !Int;xFormChan;xFormChan;?Int}] r c3 in
      let x1, c5 = receive c4 in
      (Node x1 l1 r1, c5)



treeSum :: forall x => (rec xFormChan . &{LeafC: Skip, NodeC: ?Int;xFormChan;xFormChan;!Int});x -> (Int, x)
treeSum c =
  match c with
    LeafC c1 -> (0, c1)
    NodeC c1 ->
      let x, c2 = receive c1 in
      let l, c3 = treeSum[(rec xFormChan . &{LeafC: Skip, NodeC: ?Int;xFormChan;xFormChan;!Int});x] c2 in
      let r, c4 = treeSum[!Int;x] c3 in
      let c5    = send (x+l+r) c4 in
      (x+l+r, c5)
  

-- --   fork (((transform aTree) w) >>= \x -> return (fst (x)) >> return ())

-- V1
-- Node 7 Leaf (Node 4 Leaf Leaf)
start :: Tree
start =
  let aTree = Node 3 Leaf (Node 4 Leaf Leaf) in
  let w, r  = new (rec xFormChan . +{LeafC: Skip, NodeC: !Int;xFormChan;xFormChan;?Int}) in
  let x = fork (fst[Int, Skip] (treeSum[Skip] r)) in
  (fst[Tree, Skip] (transform[Skip] aTree w))
 
{- deadlock

start :: Int
start =
  let aTree = Node 3 Leaf (Node 4 Leaf Leaf) in
  let w, r  = new (rec xFormChan . +{LeafC: Skip, NodeC: !Int;xFormChan;xFormChan;?Int}) in
  let x = fork (fst[Tree, Skip] (transform[Skip] aTree w)) in
  let size, r1 = treeSum r in
  size
-}         
  
