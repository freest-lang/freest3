type Ping : 1S = !Int ; Pong
type Pong : 1S = ?Int ; Ping

mutual { ping : Int -> Ping -> Diverge
       , pong : Pong -> Diverge
       }
ping n c = c |> send n |> pong
pong c =
  let (n, c) = receive c in
  print @Int n;
  ping (n + 1) c

main : ()
main = forkWith @Pong @Diverge (ping 0) |> pong
