


sendInt : !Int -> Skip
sendInt c = send 5 c

receiveInt : dualof (dualof (dualof !Int)) -> Int
receiveInt c =
  let (x, c) = receive c in x


main : Int
main =
  let (w,r) = new dualof !Int in
  let _     = fork @Skip \_:() 1-> sendInt r in
  receiveInt w
