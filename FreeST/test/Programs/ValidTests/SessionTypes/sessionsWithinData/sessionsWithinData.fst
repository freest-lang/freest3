data T : 1T = One EndW | Two ?Int;EndW

read : T -> Int
read t =
  case t of {
    One c -> wait c; 5,
    Two c -> receiveAndWait @Int c
  }

main : Int
main =
  let (w, r) = new @(!Int;EndC) () in
  fork @() (\_:()1-> send 10 w |> close);
  read $ Two r
