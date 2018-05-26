
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
      let l1, c3 = transform[rec xFormChan . +{LeafC: Skip, NodeC: !Int;xFormChan;xFormChan;?Int}] l c2 in
      let r1, c4 = transform[?Int;Skip] r c3 in
      -- let r1, c4 = transform[rec xFormChan . +{LeafC: Skip, NodeC: !Int;xFormChan;xFormChan;?Int}] r c3 in
      let x1, c5 = receive c4 in
      (Node x1 l1 r1, c5)



start :: Int
start = 10
         
      
  

