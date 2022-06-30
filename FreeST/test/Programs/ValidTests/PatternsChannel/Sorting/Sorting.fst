{- |
Module      :  Sorting
Description :  Server that receives a series of integer values and returns them in ascending or descending order.
Copyright   :  (c) Diogo Barros

-}

type OrderingChannel : SL = +{
  Value: !Int ; OrderingChannel ; ?Int,
  Ascending: Skip,
  Descending: Skip}

-- Send a series of integer values to the server; receive and print
-- the values in ascending or descending order
client : OrderingChannel -> ()
client c =
  let c = select Descending {- Ascending -} $
  send 9 $ select Value $
  send 2 $ select Value $
  send 7 $ select Value $
  send 1 $ select Value $
  send 4 $ select Value $
  send 5 $ select Value c in
  let (x, c) = receive c in printIntLn x;
  let (x, c) = receive c in printIntLn x;
  let (x, c) = receive c in printIntLn x;
  let (x, c) = receive c in printIntLn x;
  let (x, c) = receive c in printIntLn x;
  let (x, c) = receive c in printIntLn x

data IntList = Nil | Cons Int IntList

-- Receive a series of integer values; return them in ascending or
-- descending order
sortingServer : forall a:SL . IntList -> dualof OrderingChannel;a -> (IntList, a)
sortingServer xs c =
  match c with {
    Value c ->
      let (x, c)  = receive c in
      let (xs, c) = sortingServer[!Int;a] (Cons x xs) c in
      case xs of {
        Cons y ys -> (ys, send y c),
        -- Nil is never reached
        Nil -> (Nil, send 36042069 c)
      },
    Ascending c ->
      (quicksort (\x:Int -> (\y:Int -> x < y)) xs, c),
    Descending c ->
      (quicksort (\x:Int -> (\y:Int -> x > y)) xs, c)
  }

-- Putting it all together
main : ()
main =
  let (w, r) = new OrderingChannel in
  fork[(IntList, Skip)] (sortingServer[Skip] Nil r);
  client w

-- Quicksort.  Adapted from learnyouahaskell.com. The integer sorting
-- function is a parameter.
quicksort : (Int -> Int -> Bool) -> IntList -> IntList
quicksort cmp xs =
  case xs of {
    Nil -> Nil,
    Cons x xs' ->
      let (smaller, greater) = split cmp x xs' (Nil, Nil) in
      append (quicksort cmp smaller) (Cons x (quicksort cmp greater))
  }

split : (Int -> Int -> Bool) -> Int -> IntList -> (IntList, IntList) -> (IntList, IntList)
split cmp y xs pair  =
  let (smaller, greater) = pair in
  case xs of {
    Nil -> pair,
    Cons x xs' ->
      split cmp y xs' (if cmp x y
                       then (Cons x smaller, greater)
                       else (smaller, Cons x greater))
  }

append : IntList -> IntList -> IntList
append xs ys =
  case xs of {
    Nil -> ys,
    Cons x xs' -> Cons x (append xs' ys)
  }
