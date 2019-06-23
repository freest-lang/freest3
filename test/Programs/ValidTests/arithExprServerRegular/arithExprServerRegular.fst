{-|
 The (linear) protocol for an arithmetic stream

type Stream = +{
	Add: Stream,
	Mult: Stream,
	Const: !Int. Stream,
	EOS: ?Int. end
}
-}

-- A sample client: (5*4)+(2*3)
client : rec x: SL. +{Add: x, Mult: x, Const: !Int;x, EOS: ?Int} -> Int
client c =
  -- stream the arithmetic operation
  let c = select c Const in
  let c = send c 5 in
  let c = select c Const in
  let c = send c 4 in
  let c = select c Mult in
  let c = select c Const in
  let c = send c 2 in
  let c = select c Const in
  let c = send c 3 in
  let c = select c Mult in
  let c = select c Add in
  let c = select c EOS in
  -- read the result
  let x, c = receive c in
  -- and return it
  x

{-|
  An easy consumer: counts the number of nodes in the stream.  Copes
  with any stream, independent of the fact that it may or may not
  represent a well formed arithmetic expression.
-}
size : rec x: SL. &{Add: x, Mult: x, Const: ?Int;x, EOS: !Int} -> Int -> Skip
size s n =
  match s with {
    Add s   -> size s (n + 1),
    Mult s  -> size s (n + 1),
    Const s -> let _, s = receive s in size s (n + 1),
    EOS s   -> send s n
  }

-- A sample interaction: counting the number of nodes in a stream;
-- expect 7 on the console.
main : Int
main =
  let c, s = new rec x: SL. +{Add: x, Mult: x, Const: !Int;x, EOS: ?Int} in
  let _ = fork (size s 0) in
  client c
