{-|
 The (regular) protocol for an arithmetic stream

type Stream = +{
	Add: Stream,
	Mult: Stream,
	Const: !Int. Stream,
	Done: ?Int. end
}
-}

type MathServer = +
  { Add: MathServer
  , Mult: MathServer
  , Const: !Int;MathServer
  , Done: ?Int
  }

-- A sample client: (5*4)+(2*3)
client : MathServer;Close -> Int
client c =
  -- stream the arithmetic operation
  select Const c |> send 5 |>
  select Const |> send 4 |>
  select Mult |>
  select Const |> send 2 |>
  select Const |> send 3 |>
  select Mult |>
  select Add |>
  select Done |>
  -- read the result and return it
  receiveAndClose @Int

{-|
  An arithmetic stream evaluator.
  The evaluator reads from a stream and returns the result.
-}

data IntList = Nil | Cons Int IntList

evaluate : dualof MathServer;Wait -> IntList 1-> Wait
evaluate (Const s) l = let (n, s) = receive s in evaluate s (Cons n l)
evaluate (Add   s) (Cons x (Cons y l)) = evaluate s (Cons (x + y) l)
evaluate (Mult  s) (Cons x (Cons y l)) = evaluate s (Cons (x * y) l)
evaluate (Done  s) (Cons x Nil) = send x s

-- A sample interaction: evaluating an arithmetic expression;
-- expect 26 on the console.
main : Int
main =
  let (c, s) = new @(MathServer;Close) () in
  fork (\_:() 1-> evaluate s Nil |> wait);
  client c
