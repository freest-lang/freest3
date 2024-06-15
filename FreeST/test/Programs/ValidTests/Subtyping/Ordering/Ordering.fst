{- |
Module      :  Ordering
Description :  Server that orders lists and returns them
Copyright   :  (c) Diogo Barros

This example shows a server that receives a list, lets the user choose which
order to use in quicksort and returns the ordered list.

-}

import List

-- quicksort option to sort by ascending
asc : Int -> Int -> Bool
asc x i = x < i

-- quicksort option to sort by descending
desc : Int -> Int -> Bool
desc x i = not (asc x i)

-- Divides a list into lesser and greater or equal,given a pivot
quicksortDivide' : Int -> [Int] -> ([Int], [Int]) -> (Int -> Int -> Bool) -> ([Int], [Int])
quicksortDivide' i [] d direction         = d
quicksortDivide' i (x::xs) d direction = 
  let (smaller, greater) = d in
  if direction x i
    then quicksortDivide' i xs (x::smaller, greater) direction
    else quicksortDivide' i xs (smaller, x::greater) direction

-- Facade function to quicksortDivide
quicksortDivide : Int -> [Int] -> (Int -> Int -> Bool) -> ([Int], [Int])
quicksortDivide i list direction = quicksortDivide' i list ([], []) direction

-- Translated from Haskell code at learnyouahaskell.com/recursion
--   The function it receives determines which order it sorts,
--   ascending or descending
quicksort : [Int] -> (Int -> Int -> Bool) -> [Int]
quicksort []      direction = []
quicksort (x::xs) direction = 
  let (smaller, greater) = quicksortDivide x xs direction in
  quicksort smaller direction ++ (x::quicksort greater direction)

-- ==================== Server ====================

type OrderingServer = &{Vals: ?Int;OrderingServer;!Int, Asc: Skip, Desc: Skip}

-- Server function
--   This server sends the list reversed
orderedServer : OrderingServer;a -> [Int] 1-> ([Int], a)
orderedServer (Asc  c) list = (quicksort list (desc), c) -- Quicksorts with descending to send it reversed
orderedServer (Desc c) list = (quicksort list (asc) , c) -- Quicksorts with  ascending to send it reversed
orderedServer (Vals c) list = 
  let (x, c)  = receive c in
  let (list, c) = orderedServer@(!Int;a) c (x::list) in
  case list of { 
    (y::ys) -> 
      let c = send y c in
      (ys, c)
    -- Nil is never reached
  }

-- Facade function to initialize server with an empty list
initOrderedServer : OrderingServer;Wait -> ()
initOrderedServer c =
  orderedServer@Wait c [] |> snd @[Int] @Wait |> wait 

-- ==================== Client ====================

type DescClient = +{Vals: !Int;DescClient;?Int, Desc: Skip}
type AscClient  = +{Vals: !Int;AscClient;?Int , Asc : Skip}

-- Function to send a list and receive it ordered
--  direction : Bool - is used to determine if Asc(True) or
--                     Desc(False) is selected
orderAsc : AscClient;a -> [Int] 1-> (a, [Int])
orderAsc c []      = (select Asc c , [])
orderAsc c (x::xs) = let c          = select Vals c in
                     let c          = send x c in
                     let (c, rList) = orderAsc@(?Int;a) c xs in
                     let (y, c)     = receive c in
                     (c, y::rList)

orderDesc : DescClient;a -> [Int] 1-> (a, [Int])
orderDesc c []      = (select Desc c , [])
orderDesc c (x::xs) = let c          = select Vals c in
                      let c          = send x c in
                      let (c, rList) = orderDesc@(?Int;a) c xs in
                      let (y, c)     = receive c in
                      (c, y::rList)

aList : [Int]
aList = [4,1,3,2]

-- Simple client using Asc option
ascClient : AscClient;Close -> [Int]
ascClient c =
  let (c, rList) = orderAsc@Close c aList in
  close c; rList


-- Simple client using Desc option
descClient : DescClient;Close -> [Int]
descClient c =
  let (c, rList) = orderDesc@Close c aList in
  close c; rList

-- ==================== Main ====================

main : [Int]
main =
  let (w, r) = new @(AscClient;Close) () in
  fork (\_:() 1-> initOrderedServer r) ;
  ascClient w;

  let (w, r) = new @(DescClient;Close) () in
  fork (\_:() 1-> initOrderedServer r) ;
  descClient w
