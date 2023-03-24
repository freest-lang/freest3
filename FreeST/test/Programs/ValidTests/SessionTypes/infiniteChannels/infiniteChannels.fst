-- Creates an unbounded number of channels; diverges
write : !Int;End -> Int 1-> ()
write c n =
  let c = send n c in
  print @Int n ;
  let (r, w) = new @(!Int;End) () in
  fork (\_:()1-> receiveAndClose @Int w); 
  write r (n + 1) ;
  close c

main : ()
main =
  let (r, w) = new @(!Int;End) () in
  fork (\_:()1-> receiveAndClose @Int w);
  write r 0
