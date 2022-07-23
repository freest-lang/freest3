myfork : ∀a: 1T. a -> ()
myfork = fork

main : Int
main =
  let (r, w) = new ?Int;End in
  myfork  @() (send 5 w & close) ;
  let (n, r) = receive r in
  close r;
  n
