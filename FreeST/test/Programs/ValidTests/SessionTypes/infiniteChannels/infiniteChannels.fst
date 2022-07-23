-- Creates an unbounded number of channels; diverges
write : !Int;End 1-> Int *-> Char
write c n =
  let _ = send n c in
  printIntLn n ;
  let (r, w) = new !Int in
  fork @() (let (_, w) = receive w in close w); 
  write r (n + 1)
  & close

main : Char
main =
  let (r, w) = new !Int;End in
  fork @() (let (_, w) = receive w in close w);
  write r 0
