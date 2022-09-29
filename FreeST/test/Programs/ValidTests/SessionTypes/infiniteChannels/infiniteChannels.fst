-- Creates an unbounded number of channels; diverges
write : !Int;End -> Int 1-> ()
write c n =
  let c = send n c in
  printIntLn n ;
  let (r, w) = new !Int;End in
  fork @() (\_:()1-> let (_, w) = receive w in close w); 
  write r (n + 1) ;
  close c

main : ()
main =
  let (r, w) = new !Int;End in
  fork @() (\_:()1-> let (_, w) = receive w in close w);
  write r 0
