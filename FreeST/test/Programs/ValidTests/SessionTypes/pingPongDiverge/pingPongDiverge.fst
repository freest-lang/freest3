type Ping = !Int ; Pong
type Pong = ?Int ; Ping

ping : Int -> Ping -> Diverge
ping n c = c |> send n |> pong

pong : Pong -> Diverge
pong c =
  let (n, c) = receive c in
  print @Int n;
  ping (n + 1) c

main : ()
main = forkWith @Pong @Diverge (ping 0) |> pong
