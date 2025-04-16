{-|
 The (regular) protocol for an arithmetic stream

type Stream = +{
	Add: Stream,
	Mult: Stream,
	Const: !Int. Stream,
	EOS: ?Int. end
}
-}

type StreamServer = &{  Add  : StreamServer
                      , Mult : StreamServer
                      , Const: ?Int; StreamServer
                      , EOS  : !Int; Close 
                      }


{-|
  An easy consumer: counts the number of nodes in the stream.  Copes
  with any stream, independent of the fact that it may or may not
  represent a well formed arithmetic expression.
-}
size : Int -> StreamServer -> ()
size n s =
  match s with {
    Add s   -> size (n + 1) s,
    Mult s  -> size (n + 1) s,
    Const s -> let (_, s) = receive s in size (n + 1) s,
    EOS s   -> send n s |> close
  }

-- A sample interaction: counting the number of nodes in a stream;
-- expect 7 on the console.
main : ()
main =
  newHcServer @StreamServer ("127.0.0.1", "8081") |>
  size 0

