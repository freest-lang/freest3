data Tree = Leaf | Node Int Tree Tree

flatten : Tree -> rec x:SL. +{Root: !Int;x, Node: x, EOS: Skip} -> rec x:SL. +{Root: !Int;x, Node: x, EOS: Skip}
flatten tree c =
  case tree of {
    Leaf ->
      select EOS c,
    Node x l r ->
      let c = select Root c in
      let c = send c x      in
      let c = select Node c in
      let c = flatten l c   in
      let c = select Node c in
              flatten r c
  }

raise : rec x:SL. +{Root: ?Int;x, Node: x, EOS: Skip} -> (Tree, rec x:SL. +{Root: ?Int;x, Node: x, EOS: Skip})
raise c =
  match c with {
    EOS c -> (Leaf, c),
    Root c ->
      let x, c = receive c in
      let l, c = raise c in
      let r, c = raise c in
      (Node x l r, c)
  }

