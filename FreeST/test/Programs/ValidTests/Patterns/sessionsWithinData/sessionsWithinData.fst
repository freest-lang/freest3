data T : 1T = One Skip | Two ?Int

read : T -> Int
read (One _) = 5
read (Two c) = let (x, _) = receive c in x

main : Int
main =
  let (w, r) = new !Int in
  fork (\_:() 1-> read $ Two r);
  let _ = send 5 w in
  10
