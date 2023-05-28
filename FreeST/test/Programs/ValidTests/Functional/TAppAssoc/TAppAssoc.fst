g : !Int;EndC -> EndC
g c =  (send  @Int 5)  @EndC c

main : ()
main = 
  let (x, y) = new @(!Int;EndC)  () in
  let _ = fork @Int \_:() 1-> (receiveAndWait @Int y) in
  g x |> close
   
