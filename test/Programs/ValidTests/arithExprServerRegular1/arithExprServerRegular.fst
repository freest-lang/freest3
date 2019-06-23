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
  let x, _ = receive c in
  -- and return it
  x

{-|
  An arithmetic stream evaluator.
  The evaluator reads from a stream and returns the result.
-}

data IntList = Nil | Cons Int IntList

evaluate : rec x: SL. &{Add: x, Mult: x, Const: ?Int;x, EOS: !Int} ->
           IntList ->
           Skip
evaluate s l =
  match s with {
    Const s -> let n, s = receive s in evaluate s (Cons n l),
    Add s   -> let p, l = head2 l in let x, y = p in evaluate s (Cons (x + y) l),
    Mult s  -> let p, l = head2 l in let x, y = p in evaluate s (Cons (x * y) l),
    EOS s   -> send s (headSingleton l)
  }

head2 : IntList -> ((Int, Int), IntList)
head2 l =
  case l of {
    Nil -> ((err, err), Nil),             -- Error: Empty stack on add/mult (left operand)
    Cons n l -> case l of {
                  Nil -> ((n, err), Nil), -- Error: Empty stack on add/mult (right operand)
                  Cons m l -> ((n, m), l)
                }
  }

headSingleton : IntList -> Int
headSingleton l =
  case l of {
    Nil -> err,                   -- Error: Empty stack at end of stream
    Cons n l -> case l of {
                  Nil -> n,
                  Cons _ _ -> err -- Error: Non empty stack after end of stream
                }
  }

err : Int
err = -1

-- A sample interaction: evaluating an arithmetic expression;
-- expect 26 on the console.
main : Int
main =
  let c, s = new rec x: SL. +{Add: x, Mult: x, Const: !Int;x, EOS: ?Int} in
  let _ = fork (evaluate s Nil) in
  client c
  
