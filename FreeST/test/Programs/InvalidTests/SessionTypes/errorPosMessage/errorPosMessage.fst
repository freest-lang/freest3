
main : Int
main =
  let (w, r) = new !Bool in
  fork @() (\_:()-> f w);
  let (x, c) = f1 r in
  close c;
  x

type F = !Int;End

f : F -> ()
f c = send c 4 |> close

f1 : dualof F -> (Int, End)
f1 c = receive c
