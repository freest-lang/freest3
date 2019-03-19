{-

This program does not deadlock with communication buffers of size 1,
which is what you get with an implementation with two MVars per
channel. In a typical synchronous (unbuffered) semantics, the program
should deadlock.

-}

writer : !Char -> !Bool -> Skip
writer w1 w2 =
  let w1 = send w1 'c' in
  send w2 False

reader : ?Char -> ?Bool -> Bool
reader r1 r2 =
  let x, r2 = receive r2 in
  let y, r1 = receive r1 in
  x

main : Bool
main =
  let w1, r1 = new !Char in
  let w2, r2 = new !Bool in
  let u = fork (writer w1 w2) in
  reader r1 r2
