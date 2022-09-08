apply : (?Int;End -> (Int, End)) -> ?Int;End -> (Int, End)
apply f = f

main : ()
main =
    let (r, w) = new ?Int;End in
    fork  @() (close (snd @Int @End (apply (receive  @Int @End) r))) ;
    send 5 w & close
