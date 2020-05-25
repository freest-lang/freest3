


sendInt : !Int -> Skip
sendInt c = send c 5

receiveInt : dualof (dualof (dualof !Int)) -> Int
receiveInt c =
  let (x, c) = receive c in x


main : Int
main =
  let (w,r) = new dualof !Int in
  let _     = fork (sendInt r) in
  receiveInt w
