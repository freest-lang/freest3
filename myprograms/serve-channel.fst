type Protocol = rec x . +{Continue: forall a . a ; x, Done: Close}

type Session = forall a . !a ; Skip

client : forall a . [a] -> Protocol 1-> ()
client _ [] c =
  --putStrLn "Hi from client []";
  c |> select Done |> close 
client t (x::xs) c =
  --putStrLn "Hi from client l";
  c |> select Continue |> sendType (dualof Session) |> sendType t |> send x |> client xs

server : dualof Protocol 1-> ()
server c =
  match c with {
	  Continue c1 ->
      let (t, c2) = receiveType c1 in
      match t with {
        dualof Session -> 
          let (t, c3) = receiveType c2 in
          let (v, c4) = receive c3 in
          print @t v,
          server c4
        _ ->
          print @String "server undefined for this session type"
      };
    Done c1 ->
      print @String "Server closed";
      close c1
  }

main : ()
main =
  let (c, s) = new @Protocol () in
  fork @() (\_:() 1-> (server s));
  client @Int [1,4,3,9] c


