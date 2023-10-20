data T = One Skip | Two ?Int;End

read : T -> Int
read (One _) = 5
read (Two c) = receiveAndClose @Int c

main : Int
main =
  let (w, r) = new @(!Int;End) () in
  fork (\_:() 1-> read $ Two r);
  send 5 w |> close;
  10
