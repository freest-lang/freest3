main : Bool
main =
  let (w, r) = new @(?Int;!Bool;EndW) () in
  fork @() (\_:()1-> client w);
  r |> send (-5) |> receiveAndClose @Bool


client : ?Int;!Bool;EndW -> ()
client c =
  let (n, c) = receive c in
  c |> send (n >= 0) |> wait
