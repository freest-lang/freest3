{- |
Module      :  Crisscross
Description :  Crossed writes and reads on two different channels
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos,

This program does not deadlock with communication buffers of size 1,
which is what you get with an implementation with two MVars per
channel or with asynchronous channels. In a typical synchronous
(unbuffered) semantics, the program deadlocks.

-}

writer : !Char;Close -> !Bool;Close 1-> ()
writer w1 w2 =
  w1 |> send 'c' |> close; 
  w2 |> send False |> close 

reader : ?Char;Wait -> ?Bool;Wait 1-> Bool
reader r1 r2 =
  receiveAndWait @Char r1;
  receiveAndWait @Bool r2 

main : Bool
main =
  let (w1, r1) = new @(!Char;Close) () in
  let (w2, r2) = new @(!Bool;Close) () in
  fork @() (\_:()1-> writer w1 w2);
  reader r1 r2
