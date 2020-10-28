{- |
Module      :  Crisscross
Description :  Crossed writes and reads on two different channels
Copyright   :  (c) Bernardo Almeida, Vasco T. Vasconcelos, Andreia Mordido

This program does not deadlock with communication buffers of size 1,
which is what you get with an implementation with two MVars per
channel or with asynchronous channels. In a typical synchronous
(unbuffered) semantics, the program deadlocks.

-}

writer : !Char -> !Bool -> Skip
writer w1 w2 =
  let w1 = send 'c' w1 in
  send False w2

reader : ?Char -> ?Bool -> Bool
reader r1 r2 =
  let (x, r2) = receive r2 in
  let (y, r1) = receive r1 in
  x

main : Bool
main =
  let (w1, r1) = new !Char in
  let (w2, r2) = new !Bool in
  let u = fork (sink (writer w1 w2)) in
  reader r1 r2

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
