type StreamService = rec x . +{Stream: forall a . !a ; x, Done: Close}

client : forall a:*T . [a] -> StreamService 1-> ()
client [] c =
  --putStrLn "Hi from client []";
  c |> select Done |> close 
client (x::xs) c =
  --putStrLn "Hi from client l";
  let c2 = c |> select Stream |> sendType a |> send x in
  --print @Int sum;
  client xs c2

server : dualof StreamService 1-> ()
server c =
  match c with {
    Stream c1 ->
      let (t, c2) = receiveType c1 in
      let (v, c3) = receive c2 in
      print @t v,
      server c3
    Done c1 ->
      print @String "Server closed";
      close c1
  }

main : ()
main =
  let (c, s) = new @StreamService () in
  fork @() (\_:() 1-> (server s));
  client @Int [1,4,3,9] c

