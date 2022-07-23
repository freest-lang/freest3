data T = One End | Two ?Int;End

read : T -> Int
read t =
  case t of {
    One c -> close c; 5,
    Two c -> let (x, c) = receive c in close c; x
  }

main : Int
main =
  let (w, r) = new !Int;End in
  fork @() (send 10 w & close);
  read $ Two r
