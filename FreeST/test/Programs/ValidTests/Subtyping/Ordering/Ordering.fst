{- |
Module      :  Ordering
Description :  Server that orders lists and returns them
Copyright   :  (c) Diogo Barros

This example shows a server that receives a list, lets the user choose which
order to use in quicksort and returns the ordered list.

-}

-- ==================== Structures ====================

data IntList = Nil | Cons Int IntList

type OrderingServer  : 1S = &{Vals: ?Int;OrderingServer;!Int, Asc: Skip, Desc: Skip}
type DescClient  : 1S = +{Vals: !Int;DescClient;?Int, Desc: Skip}
type AscClient   : 1S = +{Vals: !Int;AscClient;?Int , Asc : Skip}

-- ==================== Server ====================

-- Facade function to initialize server with an empty list
initOrderedServer : OrderingServer;Wait -> ()
initOrderedServer c =
  orderedServer@Wait c Nil |> snd @IntList @Wait |> wait 

-- Server function
--   This server sends the list reversed
orderedServer : forall a:1S . OrderingServer;a -> IntList 1-> (IntList, a)
orderedServer (Asc  c) list = (quicksort list (desc), c) -- Quicksorts with descending to send it reversed
orderedServer (Desc c) list = (quicksort list (asc) , c) -- Quicksorts with  ascending to send it reversed
orderedServer (Vals c) list = 
  let (x, c)  = receive c in
  let (list, c) = orderedServer@(!Int;a) c (Cons x list) in
  case list of { 
    Cons y ys -> 
      let c = send y c in
      (ys, c)
    -- Nil is never reached
  }

-- ==================== QUICKSORT ====================

-- Translated from Haskell code at learnyouahaskell.com/recursion
--   The function it receives determines which order it sorts,
--   ascending or descending
quicksort : IntList -> (Int -> Int -> Bool) -> IntList
quicksort Nil         direction = Nil
quicksort (Cons x xs) direction = 
  let (smaller, greater) = quicksortDivide x xs direction in
  listAppend (quicksort smaller direction) (Cons x (quicksort greater direction))

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
quicksortDivide' i Nil d direction         = d
quicksortDivide' i (Cons x xs) d direction = 
  let (smaller, greater) = d in
  if direction x i
    then quicksortDivide' i xs (Cons x smaller, greater) direction
    else quicksortDivide' i xs (smaller, Cons x greater) direction

-- Appends two lists
listAppend : IntList -> IntList -> IntList
listAppend Nil         ll = ll
listAppend (Cons x xs) ll = Cons x (listAppend xs ll)

-- ==================== Client ====================

-- Simple client using Asc option
ascClient : AscClient;Close -> IntList
ascClient c =
  let (c, rList) = orderAsc@Close c aList in
  close c; rList


-- Simple client using Desc option
descClient : DescClient;Close -> IntList
descClient c =
  let (c, rList) = orderDesc@Close c aList in
  close c; rList


-- ==================== Client Aux Functions ====================

-- Function to send a list and receive it ordered
--  direction : Bool - is used to determine if Asc(True) or
--                     Desc(False) is selected
orderAsc : forall a:1S . AscClient;a -> IntList 1-> (a, IntList)
orderAsc c Nil         = (select Asc c , Nil)
orderAsc c (Cons x xs) = let c          = select Vals c in
                         let c          = send x c in
                         let (c, rList) = orderAsc@(?Int;a) c xs in
                         let (y, c)     = receive c in
                         (c, Cons y rList)

orderDesc : forall a:1S . DescClient;a -> IntList 1-> (a, IntList)
orderDesc c Nil         = (select Desc c , Nil)
orderDesc c (Cons x xs) = let c          = select Vals c in
                          let c          = send x c in
                          let (c, rList) = orderDesc@(?Int;a) c xs in
                          let (y, c)     = receive c in
                          (c, Cons y rList)

-- ==================== Mock List ====================

aList : IntList
aList = Cons 4 (Cons 1 (Cons 3 (Cons 2 Nil)))


-- ==================== Main ====================

main : IntList
main =
  let (w, r) = new @(AscClient;Close) () in
  fork (\_:() 1-> initOrderedServer r) ;
  ascClient w;

  let (w, r) = new @(DescClient;Close) () in
  fork (\_:() 1-> initOrderedServer r) ;
  descClient w
