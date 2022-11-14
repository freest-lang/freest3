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

type FiniteOutStream:1S = +{More: !Int;FiniteOutStream, Enough: End}
type FiniteInStream:1S = dualof FiniteOutStream

writeValues : !Int;!Int;FiniteOutStream;End 1-> ()
writeValues c = c |> send 1 |> send 2 |> writeAll 3

writeAll : Int -> FiniteOutStream -> ()
writeAll i c =
  if i <= 40 then
     select More c |>
     send (if mod i 10 == 0 then 100 else i) |>
     writeAll (i + 1)
  else c |> select Enough |> close

readValues : ?Int;?Int;FiniteInStream -> FiniteOutStream 1-> ()
readValues from to =
  let (x, from) = receive from in
  let (y, from) = receive from in
  readAll x y from to

readAll : Int -> Int -> FiniteInStream -> FiniteOutStream 1-> ()
readAll x y from to =
  match from with {
    More from ->
      let (z, from) = receive from in
      select More to |>
      send (average3 x y z) |>
      readAll y z from,
    Enough from -> from |> close; to |> select Enough |> close
  }

collectValues : FiniteInStream -> ()
collectValues c =
  match c with {
     More c ->
       let (v, c) = receive c in
       printIntLn v ;
       collectValues c,
     Enough c -> c |> close
  }

average3 : Int -> Int -> Int -> Int
average3 x y z = (x + y + z) / 3

main : ()
main =
  let r1 = forkWith @(?Int;?Int;FiniteInStream;End) @() writeValues in
  let r2 = forkWith @(FiniteInStream;End) @() (readValues r1) in
  collectValues r2