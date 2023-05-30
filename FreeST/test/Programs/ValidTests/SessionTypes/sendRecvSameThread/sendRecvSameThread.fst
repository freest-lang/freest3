main : Int
main =
  let (w, r) = new @(!Int;EndC) () in
  let w    = send 5 w |> close in
  let (x, r) = receive r in
  wait r;
  x
