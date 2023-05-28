id' : forall a : 1T . a -> a
id' x = x

main : Int
main =
  let (w, r) = id' @(!Int;EndC, ?Int;EndW) (new @(!Int;EndC) ()) in
  let x = fork @() (\_:()1-> send 5 w |> close) in
  receiveAndWait @Int r 
