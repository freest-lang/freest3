myfork : ∀a: 1T. a -> ()
myfork = fork

main : Int
main =
  let (r, w) = new ?Int in
  myfork  @Skip (send 5 w) ;
  fst  @Int @Skip (receive r)
