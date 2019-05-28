{-|
 The (linear) protocol for an arithmetic stream

type Stream = +{
	Add: Stream,
	Mult: Stream,
	Const: !Int. Stream,
	EOS: ?Int. end
}
-}

-- A sample client: (2*3)+(4*5)
client : rec x: SL. +{Add: x, Mult: x, Const: !Int;x, EOS: ?Int} -> Int
client c =
  -- stream the arithmetic operation
  let c = select Const c in
  let c = send c 5 in
  let c = select Const c in
  let c = send c 4 in
  let c = select Mult c in
  let c = select Const c in
  let c = send c 2 in
  let c = select Const c in
  let c = send c 3 in
  let c = select Mult c in
  let c = select Add c in
  let c = select EOS c in
  -- read the result
  let x, c = receive c in
  -- and return it
  x

{-|
  An easy consumer: counts the number of nodes in the stream.  Copes
  with any stream, independent of the fact that it may or may not
  represent a well formed arithmetic expression.
-}

size : Int -> rec x: SL. &{Add: x, Mult: x, Const: ?Int;x, EOS: !Int} -> Skip
size n s =
  match s with {
    Add s   -> size (n + 1) s;
    Mult s  -> size (n + 1) s;
    Const s -> let _, s = receive s in size (n + 1) s;
    EOS s   -> send s n
  }

-- A sample interaction: counting the number of nodes in a stream;
-- expect 7 on the console.
main : Int
main =
  let c, s = new rec x: SL. +{Add: x, Mult: x, Const: !Int;x, EOS: ?Int} in
  let _ = fork (size 0 s) in
  client c
