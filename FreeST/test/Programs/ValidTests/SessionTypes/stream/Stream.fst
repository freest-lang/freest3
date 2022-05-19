{-
I wish we could write

type Stream α:ML = !α; Stream[α]

or

type Stream α:ML = μb:SL. !α;b

or

type Stream = ∀ α:SL. μb: SL. α;b

-}

-- An arbitrary Stream consumer
consumeStream : ∀ α: 1M .
  ((μb: 1S. ?α;b) -> (μb: 1S. ?α;b)) -> -- A function that consumes the head of a stream
  (μb: 1S. ?α;b) ->                    -- The stream
  ()
consumeStream f c = consumeStream[α] f (f c)

-- An arbitrary Stream producer
produceStream : ∀ α: 1M .
  ((μb: 1S. !α;b) -> (μb: 1S. !α;b)) -> -- A function that produces the head of a stream
  (μb: 1S. !α;b) ->                    -- The stream
  ()
produceStream f c = produceStream[α] f (f c)

-- 1 _ Stream of integer values
type IntStream: 1S = !Int; IntStream

writeIntStream : IntStream -> ()
writeIntStream = produceStream [Int] (\c:IntStream -> send 7 c)

readIntStream : dualof IntStream -> ()
readIntStream = consumeStream[Int]
  (\c: (μb: 1S. ?Int;b) -> let (v, c) = receive c in printInt v; c)
  -- (\c: dualof IntStream -> let (v, c) = receive c in printInt v; c)

main : ()
main =
  let (w, r) = new IntStream in
  fork[()] (writeIntStream w);
  readIntStream r
