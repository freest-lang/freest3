{-|
 The (regular) protocol for an arithmetic stream

type Stream = +{
	Add: Stream,
	Mult: Stream,
	Const: !Int. Stream,
	EOS: ?Int. end
}
-}

-- A sample client: (5*4)+(2*3)
client : (rec x: 1S. +{Add: x, Mult: x, Const: !Int;x, EOS: ?Int}) -> Int
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
  let (x, _) = receive c in
  -- and return it
  x

{-|
  An arithmetic stream evaluator.
  The evaluator reads from a stream and returns the result.
-}

data IntList = Nil | Cons Int IntList

evaluate : (rec x: 1S. &{Add: x, Mult: x, Const: ?Int;x, EOS: !Int}) ->
           IntList 1->
           Skip
evaluate (Const s) l = let (n, s) = receive s in evaluate s (Cons n l)
evaluate (Add   s) l = let (p, l) = head2 l in let (x, y) = p in evaluate s (Cons (x + y) l)
evaluate (Mult  s) l = let (p, l) = head2 l in let (x, y) = p in evaluate s (Cons (x * y) l)
evaluate (EOS   s) l = send (headSingleton l) s

head2 : IntList -> ((Int, Int), IntList)
head2 Nil                 = ((err, err), Nil) -- Error: Empty stack on add/mult (left operand)
head2 (Cons n Nil)        = ((n, err), Nil)   -- Error: Empty stack on add/mult (right operand)
head2 (Cons n (Cons m l)) = ((n, m), l)

headSingleton : IntList -> Int
headSingleton (Cons n Nil) = n
headSingleton _ = err

err : Int
err = -1

-- A sample interaction: evaluating an arithmetic expression;
-- expect 26 on the console.
main : Int
main =
  let (c, s) = new rec x: 1S. +{Add: x, Mult: x, Const: !Int;x, EOS: ?Int} in
  let _ = fork@Skip (evaluate s Nil) in
  client c
