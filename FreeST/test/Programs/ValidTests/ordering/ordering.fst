{- |
Module      :  Ordering
Description :  Server that orders lists and returns them
Copyright   :  (c) Diogo Barros

This example shows a server that receives a list, lets the user choose which
order to use in quicksort and returns the ordered list.

-}

-- ==================== Structures ====================

data IntList = Nil | Cons Int IntList

type OrderingChannel : SL = +{Vals: !Int; OrderingChannel; ?Int, Asc: Skip, Desc: Skip}

-- ==================== Server ====================

-- Facade function to initialize server with an empty list
initOrderedServer : dualof OrderingChannel -> ()
initOrderedServer c =
  let _ = orderedServer[Skip] c Nil in
  ()

-- Server function
--   This server sends the list reversed
orderedServer : forall a:SL . dualof OrderingChannel;a -> IntList -> (IntList, a)
orderedServer c list =
  match c with {
    Vals c ->
      let (x, c)  = receive c in
      let (list, c) = orderedServer[!Int;a] c (Cons x list) in
      case list of {
        Cons y ys ->
          let c = send y c in
          (ys, c),
        -- Nil is never reached
        Nil ->
          let c = send (-36042069) c in
          (Nil, c)
      },

    -- Quicksorts with descending to send it reversed
    Asc  c ->
      (quicksort list (desc), c),
    -- Quicksorts with ascending to send it reversed
    Desc c ->
      (quicksort list (asc), c)
  }


-- ==================== QUICKSORT ====================

-- Translated from Haskell code at learnyouahaskell.com/recursion
--   The function it receives determines which order it sorts,
--   ascending or descending
quicksort : IntList -> (Int -> Int -> Bool) -> IntList
quicksort list direction =
  case list of {
    Nil -> Nil,
    Cons x xs ->
      let (smaller, greater) = quicksortDivide x xs direction in
      listAppend (quicksort smaller direction) (Cons x (quicksort greater direction))
  }

-- quicksort option to sort by ascending
asc : Int -> Int -> Bool
asc x i = x < i

-- quicksort option to sort by descending
desc : Int -> Int -> Bool
desc x i = not (asc x i)

-- Facade function to quicksortDivide
quicksortDivide : Int -> IntList -> (Int -> Int -> Bool) -> (IntList, IntList)
quicksortDivide i list direction = quicksortDivide' i list (Nil, Nil) direction

-- Divides a list into lesser and greater or equal,given a pivot
quicksortDivide' : Int -> IntList -> (IntList, IntList) -> (Int -> Int -> Bool) -> (IntList, IntList)
quicksortDivide' i list d direction =
  let (smaller, greater) = d in
  case list of {
    Nil -> (smaller, greater),
    Cons x xs -> if direction x i
                 then quicksortDivide' i xs (Cons x smaller, greater) direction
                 else quicksortDivide' i xs (smaller, Cons x greater) direction
  }

-- Appends two lists
listAppend : IntList -> IntList -> IntList
listAppend l ll =
  case l of {
    Nil -> ll,
    Cons x xs -> Cons x (listAppend xs ll)
  }

-- ==================== Client ====================

-- Simple client using Asc option
ascClient : OrderingChannel -> IntList
ascClient c =
  let (c, rList) = order[Skip] c aList True in
  rList

-- Simple client using Desc option
descClient : OrderingChannel -> IntList
descClient c =
  let (c, rList) = order[Skip] c aList False in
  rList


-- ==================== Client Aux Functions ====================

-- Function to send a list and receive it ordered
--  direction : Bool - is used to determine if Asc(True) or
--                     Desc(False) is selected
order : forall a:SL . OrderingChannel; a -> IntList -> Bool -> (a, IntList)
order c sList direction =
  case sList of {
    Nil -> if direction
           then (select Asc c , Nil)
           else (select Desc c, Nil),
    Cons x xs ->
      let c          = select Vals c in
      let c          = send x c in
      let (c, rList) = order[?Int;a] c xs direction in
      let (y, c)     = receive c in
      (c, Cons y rList)
  }


-- ==================== Mock List ====================

aList : IntList
aList = Cons 4 (Cons 1 (Cons 3 (Cons 2 Nil)))


-- ==================== Main ====================

main : IntList
main =
  let (w, r) = new OrderingChannel in
  let _      = fork (initOrderedServer r) in
  descClient w
  --ascClient w
