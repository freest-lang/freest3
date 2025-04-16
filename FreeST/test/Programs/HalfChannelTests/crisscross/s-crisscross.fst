{- |
Module      :  Crisscross
Description :  Crossed writes and reads on two different channels
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos,

This program does not deadlock with communication buffers of size 1,
which is what you get with an implementation with two MVars per
channel or with asynchronous channels. In a typical synchronous
(unbuffered) semantics, the program deadlocks.

-}

reader : ?Char;Wait -> ?Bool;Wait 1-> Bool
reader r1 r2 =
  receiveAndWait @Char r1;
  receiveAndWait @Bool r2 

main : Bool
main =
  let c1 = newHcServer @(?Char ; Wait) ("127.0.0.1", "8081") in
  let c2 = newHcServer @(?Bool ; Wait) ("127.0.0.1", "8082") in
  reader c1 c2
