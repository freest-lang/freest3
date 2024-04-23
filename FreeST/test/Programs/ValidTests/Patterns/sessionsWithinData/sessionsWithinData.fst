data T = One Skip | Two ?Int;Wait

read : T -> Int
read (One _) = 5
read (Two c) = receiveAndWait @Int c

main : Int
main =
  let (w, r) = new @(!Int;Close) () in
  fork (\_:() 1-> read $ Two r);
  send 5 w |> close;
  10
