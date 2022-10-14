main : Bool
main =
  let (w, r) = new ?Int;!Bool;End in
  fork @() (\_:()1-> client w);
  r |> send (-5) |> receiveAndClose @Bool


client : ?Int;!Bool;End -> ()
client c =
  let (n, c) = receive c in
  c |> send (n >= 0) |> close
