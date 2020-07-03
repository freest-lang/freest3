-- ==================== Structures ====================

data IntList = Nil | Cons Int IntList


-- ==================== List functions ====================

-- Inserts an int maintaining an ascending order
insertOrdered : Int -> IntList -> IntList
insertOrdered i list =
  case list of {
    Nil -> Cons i Nil,
    Cons x xs -> if i < x
                 then Cons i (Cons x xs)
                 else Cons x (insertOrdered i xs)
  }

-- Reverse an IntList fliping it like a stack
reverse : IntList -> IntList
reverse list = reverseAux list Nil

-- Auxiliary function to reverse
reverseAux : IntList -> IntList -> IntList
reverseAux i o =
  case i of {
    Nil -> o,
    Cons x xs -> reverseAux xs (Cons x o)
  }


-- ==================== Server ====================

-- Facade function to initialize server with an empty list
initOrderedServer : (rec x: SL. &{Vals: ?Int; x; !Int, Asc: Skip, Desc: Skip}) -> ()
initOrderedServer c =
  let _ = orderedServer[Skip] c Nil in
  ()

-- Server function
orderedServer : forall a:SL => (rec x: SL. &{Vals: ?Int; x; !Int, Asc: Skip, Desc: Skip});a -> IntList -> (IntList, a)
orderedServer c list =
  match c with {
    Vals c ->
      let (x, c)  = receive c in
      let list      = insertOrdered x list in
      let (list, c) = orderedServer[!Int;a] c list in
      case list of {
        Nil ->
          let c = send c 0 in
          (Nil, c),
        Cons y ys ->
          let c = send c y in
          (ys, c)
      },

    Asc  c ->
      (list, c),

    Desc c ->
      ((reverse list), c)
  }


-- ==================== Client ====================

-- Simple client using Asc option
ascClient : (rec x: SL. +{Vals: !Int; x; ?Int, Asc: Skip, Desc: Skip}) -> IntList
ascClient c =
  let (c, rList) = asc[Skip] c aList in
  rList

-- Simple client using Desc option
descClient : (rec x: SL. +{Vals: !Int; x; ?Int, Asc: Skip, Desc: Skip}) -> IntList
descClient c =
  let (c, rList) = desc[Skip] c aList in
  rList


-- ==================== Client Aux Functions ====================

-- Function to send a list and order with Asc
asc : forall a:SL => (rec x: SL. +{Vals: !Int; x; ?Int, Asc: Skip, Desc: Skip}); a -> IntList -> (a, IntList)
asc c sList =
  let (c, rList) = ascAux[a] c sList in
  (c, reverse rList)

-- Auxiliary function to asc
ascAux : forall a:SL => (rec x: SL. +{Vals: !Int; x; ?Int, Asc: Skip, Desc: Skip}); a -> IntList -> (a, IntList)
ascAux c sList =
  case sList of {
    Nil ->
      let c = select c Asc in
      (c, Nil),
    Cons x xs ->
      let c          = select c Vals in
      let c          = send c x in
      let (c, rList) = ascAux[?Int;a] c xs in
      let (y, c)     = receive c in
      (c, Cons y rList)
  }


-- Function to send a list and order with Desc
desc : forall a:SL => (rec x: SL. +{Vals: !Int; x; ?Int, Asc: Skip, Desc: Skip}); a -> IntList -> (a, IntList)
desc c sList =
  let (c, rList) = descAux[a] c sList in
  (c, reverse rList)

-- Auxiliary function to desc
descAux : forall a:SL => (rec x: SL. +{Vals: !Int; x; ?Int, Asc: Skip, Desc: Skip}); a -> IntList -> (a, IntList)
descAux c sList =
  case sList of {
    Nil ->
      let c = select c Desc in
      (c, Nil),
    Cons x xs ->
      let c          = select c Vals in
      let c          = send c x in
      let (c, rList) = descAux[?Int;a] c xs in
      let (y, c)     = receive c in
      (c, Cons y rList)
  }


-- ==================== Mock List ====================

aList : IntList
aList = Cons 4 (Cons 1 (Cons 3 (Cons 2 Nil)))


-- ==================== Main ====================

main : IntList
main =
  let (r, w) = new rec x: SL. &{Vals: ?Int; x; !Int, Asc: Skip, Desc: Skip} in
  let _      = fork (initOrderedServer r) in
  descClient w
