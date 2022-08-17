{- |
Module      :  Crisscross
Description :  Crossed writes and reads on two different channels
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos,

This program does not deadlock with communication buffers of size 1,
which is what you get with an implementation with two MVars per
channel or with asynchronous channels. In a typical synchronous
(unbuffered) semantics, the program deadlocks.

-}

writer : !Char;End -> !Bool;End 1-> ()
writer w1 w2 =
  let w1 = send 'c' w1 in 
  let w2 = send False w2 in 
  close w1; close w2 

reader : ?Char;End -> ?Bool;End 1-> Bool
reader r1 r2 =
  let (x, r2) = receive r2 in
  let (_, r1) = receive r1 in
  close r1; close r2; 
  x

main : Bool
main =
  let (w1, r1) = new !Char;End in
  let (w2, r2) = new !Bool;End in
  fork @() (writer w1 w2);
  reader r1 r2
