data T = One Skip | Two ?Int

read : T -> Int
read t =
  case t of {
    One _ -> 5,
    Two c -> let (x, _) = receive c in x
  }

sink : Int -> ()
sink _ = ()

main : Int
main =
  let (w, r) = new !Int in
  (fork $ sink $ read $ Two r) ;
  let _ = send w 5 in
  10
