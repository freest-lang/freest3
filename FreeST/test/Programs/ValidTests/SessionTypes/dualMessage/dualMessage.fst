sendInt : !Int;EndC -> ()
sendInt c = send 5 c |> close

receiveInt : dualof (dualof (dualof !Int;EndW)) -> Int
receiveInt c = receiveAndWait @Int c


main : Int
main =
  let (w,r) = new @(dualof !Int;EndW) () in
  fork @() (\_:() 1-> sendInt r);
  receiveInt w
