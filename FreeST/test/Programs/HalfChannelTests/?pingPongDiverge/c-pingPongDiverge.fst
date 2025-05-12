type Ping = !Int ; Pong
type Pong = ?Int ; Ping

ping : Int -> Ping -> Diverge
ping n c = c |> send n |> pong

and pong : Pong -> Diverge
pong c =
  let (n, c) = receive c in
  print @Int n;
  ping (n + 1) c

main : ()
main = 
  newHcClient1 @Ping("127.0.0.1", "8081") |>
  ping 0

