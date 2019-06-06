data Tree = Leaf | Node Int Tree Tree

sendTree : forall α => Tree -> (rec x . +{NodeC: !Int;x;x, LeafC: Skip}); α -> α
sendTree t c =
  case t of {
    Leaf ->
      select LeafC c,
    Node x l r ->
      let c = select NodeC c in
      let c = send x c in
      let c = sendTree [(rec x . +{NodeC: !Int;x;x, LeafC: Skip}); x] l c in
      let c = sendTree [a] r c in
      c
  }

main : Int
main = 19
