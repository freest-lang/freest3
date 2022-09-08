{-|
 The (regular) protocol for an arithmetic stream

type Stream = +{
	Add: Stream,
	Mult: Stream,
	Const: !Int. Stream,
	EOS: ?Int. end
}
-}

type StreamClient: 1S = +{ Add  : StreamClient
                         , Mult : StreamClient
                         , Const: !Int; StreamClient
                         , EOS  : ?Int; End 
                         }
type StreamServer: 1S = dualof StreamClient

-- A sample client: (5*4)+(2*3)
client : StreamClient -> Int
client c =
  -- stream the arithmetic operation
  let c = select Const c in
  let c = send 5 c in
  let c = select Const c in
  let c = send 4 c in
  let c = select Mult c in
  let c = select Const c in
  let c = send 2 c in
  let c = select Const c in
  let c = send 3 c in
  let c = select Mult c in
  let c = select Add c in
  let c = select EOS c in
  -- read the result
  let (x, c) = receive c in
  -- and return it
  close c; 
  x

{-|
  An easy consumer: counts the number of nodes in the stream.  Copes
  with any stream, independent of the fact that it may or may not
  represent a well formed arithmetic expression.
-}
size : StreamServer -> Int 1-> ()
size s n =
  match s with {
    Add s   -> size s (n + 1),
    Mult s  -> size s (n + 1),
    Const s -> let (_, s) = receive s in size s (n + 1),
    EOS s   -> send n s & close
  }

-- A sample interaction: counting the number of nodes in a stream;
-- expect 7 on the console.
main : Int
main =
  let (c, s) = new StreamClient in
  fork @() (\_:()1-> size s 0);
  client c
