main : Int
main =
  let (w, r) = new @(!Int;Close) () in
  let w    = send 5 w |> close in
  let (x, r) = receive r in
  wait r;
  x
