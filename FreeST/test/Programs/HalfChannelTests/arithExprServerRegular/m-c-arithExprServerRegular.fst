{-|
 The (regular) protocol for an arithmetic stream

type Stream = +{
	Add: Stream,
	Mult: Stream,
	Const: !Int. Stream,
	EOS: ?Int. end
}
-}

type StreamClient = +{ Add  : StreamClient
                         , Mult : StreamClient
                         , Const: !Int; StreamClient
                         , EOS  : ?Int; Wait 
                         }

-- A sample client: (5*4)+(2*3)
client : StreamClient -> Int
client c = c |> select Const
             |> send 5
             |> select Const
             |> send 4
             |> select Mult
             |> select Const
             |> send 2
             |> select Const
             |> send 3
             |> select Mult
             |> select Add
             |> select EOS
             |> receiveAndWait @Int


-- A sample interaction: counting the number of nodes in a stream;
-- expect 7 on the console.
main : Int
main =
  newHcClient @StreamClient (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  client
