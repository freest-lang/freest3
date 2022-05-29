{- |
Module      :  MovingAverage
Description :  Calculates the moving average
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  Bernardo Almeida

Calculates the moving average of a sequence of values, with a window of 3.

Structure:
  - One thread sends values from 0 to 40, except whenever it would send a
    multiple of 10, it sends 100.

  - Another thread receives those values and computes the moving average of the
    current value with the previous two. Then, it sends the result to the main
    thread.

  - The main thread starts both threads, receives the values of the moving
    average and prints them.

-}

-- From module Concurrency

-- | Create a new child process and a linear channel through which it can 
--   communicate with its parent process. Return the channel endpoint.
forkWith : forall a:1S b:*T. (dualof a -> b) -> a
forkWith f =
    let (x, y) = new a in
    fork $ f y;
    x

-- | Similar to forkWith but with a linear signature
forkWith1 : forall a:1S b:*T. (dualof a 1-> b) -> a
forkWith1 f =
    let (x, y) = new a in
    fork $ f y;
    x

-- This module

type FiniteOutStream:1S = +{More: !Int;FiniteOutStream, Enough:Skip}
type FiniteInStream:1S = dualof FiniteOutStream

writeValues : !Int;!Int;FiniteOutStream -> Skip
writeValues c = send 1 c & send 2 & writeAll @Skip 3

writeAll : ∀ a:1S . Int -> FiniteOutStream;a -> a
writeAll i c =
  if i <= 40 then
     select More c &
     send (if mod i 10 == 0 then 100 else i) &
     writeAll @a (i + 1)
  else select Enough c

readValues : ?Int;?Int;dualof FiniteOutStream -> FiniteOutStream 1-> Skip
readValues from to =
  let (x, from) = receive from in
  let (y, from) = receive from in
  readAll @Skip x y from to 

readAll : ∀ a:1S . Int -> Int -> dualof FiniteOutStream;a 1-> FiniteOutStream 1-> a
readAll x y from to =
  match from with {
    More from ->
      let (z, from) = receive from in
      select More to &
      send (average3 x y z) &
      readAll @a y z from,
    Enough from -> select Enough to ; from
  }

average3 : Int -> Int -> Int -> Int
average3 x y z = (x + y + z) / 3

collectValues : ∀ a:1S . dualof FiniteOutStream ; a -> a
collectValues c =
  match c with {
     More c ->
       let (v, c) = receive c in
       printIntLn v ;
       collectValues @a c,
     Enough c -> c
  }

main : Skip
main =
  let r1 = forkWith @(?Int;?Int;dualof FiniteOutStream) @Skip writeValues in
  let r2 = forkWith1 @dualof FiniteOutStream @Skip (readValues r1) in
  collectValues @Skip r2
