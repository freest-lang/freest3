data T = One End | Two ?Int;End

read : T -> Int
read t =
  case t of {
    One c -> close c; 5,
    Two c -> receiveAndClose @Int c
  }

main : Int
main =
  let (w, r) = new @!Int;End () in
  fork @() (\_:()1-> send 10 w |> close);
  read $ Two r
