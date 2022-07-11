data T = One Skip | Two ?Int

read : T -> Int
read t =
  case t of {
    One _ -> 5,
    Two c -> let (x, _) = receive c in x
  }

main : Int
main =
  let (w, r) = new !Int in
  fork @Int $ read $ Two r;
  let _ = send 5 w in
  10
