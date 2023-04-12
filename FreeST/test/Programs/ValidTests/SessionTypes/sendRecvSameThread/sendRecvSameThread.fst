main : Int
main =
  let (w, r) = new @(!Int;End) () in
  let w    = send 5 w |> close in
  let (x, r) = receive r in
  close r;
  x
