{- |
Module      :  MovingAverage
Description :  Calculates the moving average of a series of numbers
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

type FiniteOutStream:1S = +{More: !Int;FiniteOutStream, Enough: Skip}
type FiniteInStream:1S = dualof FiniteOutStream

writeValues : !Int;!Int;FiniteOutStream;End 1-> ()
writeValues c = send 1 c & send 2 & writeAll @End 3 & close

writeAll : ∀ a:1S . Int -> FiniteOutStream;a -> a
writeAll i c =
  if i <= 40 then
     select More c &
     send (if mod i 10 == 0 then 100 else i) &
     writeAll @a (i + 1)
  else select Enough c

readValues : ?Int;?Int;FiniteInStream;End -> FiniteOutStream;End 1-> ()
readValues from to =
  let (x, from) = receive from in
  let (y, from) = receive from in
  let (x, y) = readAll @End @End x y from to in 
  close x; close y

readAll : ∀ a:1S b:1S . Int -> Int -> FiniteInStream;a -> FiniteOutStream;b 1-> (a, b)
readAll x y from to =
  match from with {
    More from ->
      let (z, from) = receive from in
      select More to &
      send (average3 x y z) &
      readAll @a @b y z from,
    Enough from -> (from, select Enough to)
  }

collectValues : ∀ a:1S . FiniteInStream ; a -> a
collectValues c =
  match c with {
     More c ->
       let (v, c) = receive c in
       printIntLn v ;
       collectValues @a c,
     Enough c -> c
  }

average3 : Int -> Int -> Int -> Int
average3 x y z = (x + y + z) / 3

main : ()
main =
  let r1 = forkWith @(?Int;?Int;FiniteInStream;End) @() writeValues in
  let r2 = forkWith @FiniteInStream;End @() (readValues r1) in
  collectValues @End r2 & close
