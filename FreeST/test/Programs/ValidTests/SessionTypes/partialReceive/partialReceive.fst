apply : (?Int;EndW -> (Int, EndW)) -> ?Int;EndW -> (Int, EndW)
apply f = f

main : ()
main =
    let (r, w) = new @(?Int;EndW) () in
    fork  @() (\_:()1-> wait (snd @Int @EndW (apply (receive  @Int @EndW) r))) ;
    send 5 w |> close
