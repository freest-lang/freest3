client : ?Int;!Bool;Wait -> ()
client c =
  let (n, c) = receive c in
  c |> send (n >= 0) |> wait

main : Bool
main =
  let (w, r) = new @(?Int;!Bool;Wait) () in
  fork @() (\_:()1-> client w);
  r |> send (-5) |> receiveAndClose @Bool

