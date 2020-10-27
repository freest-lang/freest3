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
  let _ = fork $ let _ = read (Two r) in () in
  let _ = send w 5 in
  10
