data T : 1T = One Skip | Two ?Int;EndW

read : T -> Int
read (One _) = 5
read (Two c) = receiveAndWait @Int c

main : Int
main =
  let (w, r) = new @(!Int;EndC) () in
  fork (\_:() 1-> read $ Two r);
  send 5 w |> close;
  10
