{-

This program deadlocks with communication buffers of size 1, which is
what you get with an implementation with two MVars per channel. It
does not deadlock with buffers of size 2.

-}

reader : ?Char;?Char;Wait -> ?Bool;?Bool;Wait 1-> Bool
reader r1 r2 =
  let (x, r2) = receive r2 in
  let (x, r2) = receive r2 in
  let (y, r1) = receive r1 in
  let (y, r1) = receive r1 in
  wait r1; wait r2;
  x

main : Bool
main =
  let c1 = newHcServer @(?Char ; ?Char ; Wait) ("127.0.0.1", "8081") in
  let c2 = newHcServer @(?Bool ; ?Bool ; Wait) ("127.0.0.1", "8082") in
  reader c1 c2
