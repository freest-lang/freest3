{- |
Module      :  MovingAverage
Description :  Calculates the moving average
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  Bernardo Almeida

Calculates the moving average.

Structure:
  - One thread sends values from 0 to 40, except whenever it would send a
    multiple of 10, it sends 100.

  - One thread that receives that values and computes the moving average of the
    current value with the previous two. Then, it sends the result to the main
    thread.

  - The main thread starts both threads, receives the computed values and prints
    them.

-}

type Channel : SL = +{More: !Int;Channel, End:Skip}

writeValues : !Int;!Int;Channel -> Skip
writeValues c = send 1 c & send 2 & writeAll @Skip 3

writeAll : ∀ a : SL . Int -> Channel;a -> a
writeAll i c =
  if i <= 40 then
     select More c & send (if mod i 10 == 0 then 100 else i) & writeAll @a (i + 1)
  else select End c

readValues : ?Int;?Int;dualof Channel -o Channel -o Skip
readValues c1 c2 =
  let (x, c1) = receive c1 in
  let (y, c1) = receive c1 in
  readAll @Skip x y c1 c2 

readAll : ∀ a : SL . Int -> Int -> dualof Channel ; a -o Channel -o a
readAll x y c1 c2 =
  match c1 with {
    More c1 ->
      let (z, c1) = receive c1 in
      select More c2 & send (average3 x y z) &
      readAll @a y z c1,
    End c1 -> select End c2 ; c1
  }

average3 : Int -> Int -> Int -> Int
average3 x y z = (x + y + z) / 3

main : ()
main =
  let (w1, r1) = new !Int;!Int;Channel in
  let (w2, r2) = new Channel in
  fork $ writeValues w1 ;
  fork $ readValues r1 w2 ;
  receiveMain @Skip r2 ;
  ()

receiveMain : ∀ a:SL . dualof Channel ; a -> a
receiveMain c =
  match c with {
     More c ->
       let (v, c) = receive c in
       printIntLn v ;
       receiveMain @a c,
     End c -> c
  }
