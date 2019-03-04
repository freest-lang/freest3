{-

This program deadlocks with communication buffers of size 1, which is
what you get with an implementation with two MVars per channel. It
does not deadlock with buffers of size 2.

-}

writer : !Char;!Char -> !Bool;!Bool -> Skip
writer w1 w2 =
  let w1 = send 'c' w1 in
  let w1 = send 'd' w1 in
  let w2 = send True w2 in
  send False w2

reader : ?Char;?Char -> ?Bool;?Bool -> Bool
reader r1 r2 =
  let x, r2 = receive r2 in
  let x, r2 = receive r2 in
  let y, r1 = receive r1 in
  let y, r1 = receive r1 in
  x

start : Bool
start =
  let w1, r1 = new !Char;!Char in
  let w2, r2 = new !Bool;!Bool in
  let u = fork (writer w1 w2) in
  reader r1 r2
