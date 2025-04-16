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

main : ()
main =
  let c1 = newHcClient1 @(!Char;Close) ("127.0.0.1", "8081") in
  let c2 = newHcClient1 @(!Bool;Close) ("127.0.0.1", "8082") in
  writer c1 c2 
