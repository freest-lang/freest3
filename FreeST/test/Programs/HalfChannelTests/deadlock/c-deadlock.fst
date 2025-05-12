{-

This program deadlocks with communication buffers of size 1, which is
what you get with an implementation with two MVars per channel. It
does not deadlock with buffers of size 2.

-}

writer : !Char;!Char;Close -> !Bool;!Bool;Close 1-> ()
writer w1 w2 =
  let w1 = send 'c' w1 |> send 'd' in
  let w2 = send True w2 |> send False in 
  close w1; close w2 

main : ()
main =
  let c1 = newHcClient1 @(!Char;!Char;Close) ("127.0.0.1", "8081") in
  let c2 = newHcClient1 @(!Bool;!Bool;Close) ("127.0.0.1", "8082") in
  writer c1 c2
