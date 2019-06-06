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
  An arithmetic stream evaluator.
  The evaluator reads from a stream and return the result.
-}

size : rec x: SL. &{Add: x, Mult: x, Const: ?Int;x, EOS: !Int} -> (Int, rec x: SL. &{Add: x, Mult: x, Const: ?Int;x, EOS: !Int} -> Skip)
size s =
  match s with {
    Add s   -> let n, s = size s in (n + 1, s),
    Mult s  -> let n, s = size s in (n + 1, s),
    Const s -> let _, s = receive s in let n, s = size s in (n + 1, s),
    EOS s   -> let _ = send s n in 5
  }

-- A sample interaction: counting the number of nodes in a stream;
-- expect 7 on the console.
main : Int
main =
  let c, s = new rec x: SL. +{Add: x, Mult: x, Const: !Int;x, EOS: ?Int} in
  let _ = fork (size 0 s) in
  client c
  
