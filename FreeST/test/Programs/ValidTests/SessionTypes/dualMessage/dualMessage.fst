


sendInt : !Int;End -> ()
sendInt c = send 5 c & close

receiveInt : dualof (dualof (dualof !Int;End)) -> Int
receiveInt c =
  let (x, c) = receive c in close c; x


main : Int
main =
  let (w,r) = new dualof !Int;End in
  fork @() $ sendInt r ;
  receiveInt w
