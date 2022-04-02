{- |
Module      :  Crisscross
Description :  Crossed writes and reads on two different channels
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos,

This program does not deadlock with communication buffers of size 1,
which is what you get with an implementation with two MVars per
channel or with asynchronous channels. In a typical synchronous
(unbuffered) semantics, the program deadlocks.

-}

writer : !Char -> !Bool -o ()
writer w1 w2 =
  let _ = send 'c' w1 in
  let _ = send False w2 in
  ()

reader : ?Char -> ?Bool -o Bool
reader r1 r2 =
  let (x, _) = receive r2 in
  let (_, _) = receive r1 in
  x

main : Bool
main =
  let (w1, r1) = new !Char in
  let (w2, r2) = new !Bool in
  fork @() (writer w1 w2);
  reader r1 r2
