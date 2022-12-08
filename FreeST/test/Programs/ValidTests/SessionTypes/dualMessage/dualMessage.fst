


sendInt : !Int;End -> ()
sendInt c = send 5 c |> close

receiveInt : dualof (dualof (dualof !Int;End)) -> Int
receiveInt c = receiveAndClose @Int c


main : Int
main =
  let (w,r) = new dualof !Int;End in
  fork @() (\_:()1-> sendInt r);
  receiveInt w
