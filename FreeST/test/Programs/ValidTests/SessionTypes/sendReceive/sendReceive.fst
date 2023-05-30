main : ()
main =
  let (w, r)  = new @(!Int;?Bool;EndC) () in
  fork (\_:()1-> client w); 
  let (n, r) = receive r in
  r |> send (n >= 0) |> wait 


client : !Int;?Bool;EndC -> Bool 
client c = c |> send 5 |> receiveAndClose @Bool
