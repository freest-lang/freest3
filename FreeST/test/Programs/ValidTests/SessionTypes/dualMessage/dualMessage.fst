sendInt : !Int;Close -> ()
sendInt c = send 5 c |> close

receiveInt : dualof (dualof (dualof !Int;Wait)) -> Int
receiveInt c = receiveAndWait @Int c


main : Int
main =
  let (w,r) = new @(dualof !Int;Wait) () in
  fork @() (\_:() 1-> sendInt r);
  receiveInt w
