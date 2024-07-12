type Service : 1S = forall a:*T . !a ; Skip

client : forall a:*T . a -> Service -> ()
client v c = sendType a c |> send v |> close

server : dualof Service -> ()
server c =
  let (t, c1) = receiveType c in
  let (v, c2) = receive c1 in
  print @t v;
  close c2

main : ()
main =
  let (c, s) = new @Service () in
  fork @() (\_:() 1-> (server s));
  client c

