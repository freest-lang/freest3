myfork : ∀ a : *T . (() 1-> a) -> ()
myfork = fork

main : Int
main =
  let (r, w) = new ?Int in
  myfork  @Skip (\_:() 1-> send 5 w);
  fst  @Int @Skip (receive r)
