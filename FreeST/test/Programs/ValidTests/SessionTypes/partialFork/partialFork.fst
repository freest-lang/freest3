myfork : ∀ a : *T . (() 1-> a) -> ()
myfork = fork

main : Int
main =
  let (r, w) = new ?Int;End in
  myfork  @() (\_:()1-> send 5 w |> close) ;
  let (n, r) = receive r in
  close r;
  n
