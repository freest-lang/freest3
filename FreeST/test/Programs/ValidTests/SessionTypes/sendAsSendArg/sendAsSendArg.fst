f1 : !Int;!Int;End -> ()
f1 c = send 5 c |> send 5 |> close

f2 : ?Int;?Int;End -> Int
f2 c =
  let (x1, c) = receive c in
  let x2      = receiveAndClose @Int c in 
  x1 + x2

main : Int
main =
  let (c1, c2) = new @!Int;!Int;End () in
  let _ = fork @() (\_:()1-> f1 c1) in
  f2 c2 -- in x
