id' : forall a : 1T . a -> a
id' x = x

main : Int
main =
  let (w, r) = id' @(!Int;End, ?Int;End) (new !Int;End) in
  let x = fork @() (\_:()1-> send 5 w |> close) in
  let (y, c) = receive r in
  close c;
  y
