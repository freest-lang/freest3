g : !Int;End -> End
g c =  (send  @Int 5)  @End c

main : ()
main = 
  let (x, y) = new @(!Int;End)  () in
  let _ = fork @Int \_:() 1-> (receiveAndClose @Int y) in
  g x |> close
   
