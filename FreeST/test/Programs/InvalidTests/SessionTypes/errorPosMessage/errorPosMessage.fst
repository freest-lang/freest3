type F : 1S = !Int;Close

f : F -> ()
f c = send 4 c |> close

f1 : dualof F -> (Int, Wait)
f1 c = receive c

main : Int
main =
  let (w, r) = new @(!Bool;Close) () in
  fork @() (\_:()-> f w);
  let (x, c) = f1 r in
  close c;
  x
