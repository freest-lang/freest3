data Tree = Leaf | Node Int Tree Tree

type TreeChannel : 1S = forall p : (bot,top) =>
    &p{LeafC: Skip, 
       NodeC: ?p+1 Int; TreeChannel; TreeChannel}

receiveTree : forall p : (bot, top) => forall a:1S . TreeChannel; a ->[top,top] (Tree, a)
receiveTree =
  forall p : (bot, top) =>
  \c: forall a:1S . TreeChannel; a ->
  match (inst c) with {
    LeafC c ->
      (Leaf, c),
    NodeC c ->
      let (x, c) = receive c in
      let (left, c) = (receiveTree{priority c}) @(TreeChannel;a) c in
      let (right, c) = (receiveTree{priority c}) @a c in
      (Node x left right, c)
  }

sendTree : forall p : (bot, top) => Tree ->[top,top] TreeChannel; a ->[top,top] a
sendTree =
  forall p : (bot, top) =>
  \t: Leaf ->
  \c: TreeChannel; a ->
      select LeafC c
sendTree =
  forall p : (bot, top) =>
  \t: Node x:Int l:Tree r:Tree ->
  \c: TreeChannel; a ->
       let c = select NodeC c in
       let c = send x c in
       let c = (sendTree{priority c})  @(TreeChannel ; a) l in
       (sendTree{priority c}) @a r
--       select NodeC c
--       |> send x
--       |> (sendTree{priority c})  @(TreeChannel ; a) l
--       |> (sendTree{priority c}) @a r


aTree : Tree
aTree = Node 7 (Node 5 Leaf Leaf) (Node 9 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))

main : () 
main =
  let (writer, reader) = new @(TreeChannel) {1,2} in
  fork  @() (\_:()1-> sendTree  @Skip aTree writer);
  let (tree, reader) = receiveTree  @Skip reader in 
  tree