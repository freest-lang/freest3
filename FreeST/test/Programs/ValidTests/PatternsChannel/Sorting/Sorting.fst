{- |
Module      :  Sorting
Description :  Server that receives a series of integer values and returns them in ascending or descending order.
Copyright   :  (c) Diogo Barros

-}

type OrderingChannel = +{
  Value: !Int ; OrderingChannel ; ?Int,
  Ascending: Skip,
  Descending: Skip}

-- Quicksort.  Adapted from learnyouahaskell.com. The integer sorting
-- function is a parameter.

append : IntList -> IntList -> IntList
append Nil          ys = ys
append (Cons x xs') ys = Cons x (append xs' ys)

split : (Int -> Int -> Bool) -> Int -> IntList -> (IntList, IntList) -> (IntList, IntList)
split cmp y Nil          pair = pair
split cmp y (Cons x xs') pair = 
  let (smaller, greater) = pair in
  split cmp y xs' (if cmp x y
                    then (Cons x smaller, greater)
                    else (smaller, Cons x greater))

quicksort : (Int -> Int -> Bool) -> IntList -> IntList
quicksort cmp Nil = Nil
quicksort cmp (Cons x xs') =
  let (smaller, greater) = split cmp x xs' (Nil, Nil) in
  append (quicksort cmp smaller) (Cons x (quicksort cmp greater))

-- Send a series of integer values to the server; receive and print
-- the values in ascending or descending order
client : OrderingChannel;Close -> ()
client c =
  let c = select Descending {- Ascending -} $
  send 9 $ select Value $
  send 2 $ select Value $
  send 7 $ select Value $
  send 1 $ select Value $
  send 4 $ select Value $
  send 5 $ select Value c in
  let (x, c) = receive c in print @Int x;
  let (x, c) = receive c in print @Int x;
  let (x, c) = receive c in print @Int x;
  let (x, c) = receive c in print @Int x;
  let (x, c) = receive c in print @Int x;
  let (x, c) = receive c in print @Int x;
  close c

data IntList = Nil | Cons Int IntList

-- Receive a series of integer values; return them in ascending or
-- descending order
sortingServer : IntList -> dualof OrderingChannel;a -> (IntList, a)
sortingServer xs (Ascending  c) = (quicksort (\x:Int -> (\y:Int -> x < y)) xs, c)
sortingServer xs (Descending c) = (quicksort (\x:Int -> (\y:Int -> x > y)) xs, c)
sortingServer xs (Value      c) =
  let (x, c)  = receive c in
  let (xs, c) = sortingServer@(!Int;a) (Cons x xs) c in
  case xs of { Cons y ys -> (ys, send y c) }

-- Putting it all together
main : ()
main =
  let (w, r) = new @(OrderingChannel;Close) () in
  fork @() (\_:()1-> sortingServer @Wait Nil r |> snd @IntList @Wait |> wait);
  client w