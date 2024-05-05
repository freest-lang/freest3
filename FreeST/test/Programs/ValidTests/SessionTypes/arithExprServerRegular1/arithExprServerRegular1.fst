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
                          , Const: !Int ; StreamClient
                          , EOS  : ?Int ; Close}
type StreamServer = dualof StreamClient

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
             |> receiveAndClose @Int

{-|
  An arithmetic stream evaluator.
  The evaluator reads from a stream and returns the result.
-}

data IntList = Nil | Cons Int IntList

evaluate : StreamServer -> IntList 1-> ()
evaluate s l =
  match s with {
    Const s -> let (n, s) = receive s in evaluate s (Cons n l),
    Add s   -> let (p, l) = head2 l in let (x, y) = p in evaluate s (Cons (x + y) l),
    Mult s  -> let (p, l) = head2 l in let (x, y) = p in evaluate s (Cons (x * y) l),
    EOS s   -> send (headSingleton l) s |> wait
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
  let (c, s) = new @StreamClient () in
  let _ = fork @() (\_:()1-> evaluate s Nil) in
  client c
