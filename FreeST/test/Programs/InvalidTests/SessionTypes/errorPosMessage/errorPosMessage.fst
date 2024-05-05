
main : Int
main =
  let (w, r) = new @(!Bool;Close) () in
  fork @() (\_:() 1-> f w);
  let (x, c) = f1 r in
  wait c;
  x

type F  = !Int;Close

f : F -> ()
f c = send 4 c |> close

f1 : dualof F -> (Int, Wait)
f1 c = receive c
