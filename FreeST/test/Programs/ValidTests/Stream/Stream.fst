{-
I wish we could write

type Stream α:SL = α; Stream[α]

or

type Stream α:SL = rec b: SL. α;b

or

type Stream = forall α:SL . rec b: SL. α;b

Note: this is not exactly a stream for α can be instantiated with
!Char ; ?Bool forcing data to flow on both directions.
-}

-- An arbitrary Stream consumer
consumeStream : ∀ α:ML .
  (α;(μ b: SL. ?α;b) -> (μ b: SL. ?α;b)) ->   -- A function that consumes the head of a stream
  (μ b: SL. ?α;b) ->                           -- The stream
  ()
consumeStream f c = consumeStream[α] f (f c)

-- An arbitrary Stream producer
produceStream : ∀ α:ML .
  (α;(μ b: SL. !α;b) -> (μ b: SL. !α;b)) ->   -- A function that produces the head of a stream
  (μ b: SL. !α;b) ->                           -- The stream
  ()
produceStream f c = consumeStream[α] f (f c)

-- 1 _ Stream out-integer values
type IntStream : SL = !Int; IntStream

-- Write on an int on a channel; return the continuation channel
-- writeInt : ∀ β:SL . !Int; β -> β
-- writeInt c = send 7 c

writeIntStream : IntStream -> ()
writeIntStream = produceStream [!Int] (\c:IntStream -> send 7 c)

-- Read from an int stream
readInt : ∀ β:SL . ?Int; β -> β
readInt c = let (v, c) = receive c in printInt v; c

readIntStream : dualof IntStream -> ()
readIntStream = consumeStream[?Int] (\c:dualof IntStream -> let (v, c) = receive c in printInt v; c)

-- Run an int stream
mainIntStream : ()
mainIntStream =
  let (w, r) = new IntStream in
  fork[()] (writeIntStream w);
  readIntStream r

{-
-- 2 _ Stream of out-char-in-bool values
type OutCharInBoolStream : SL = !Char; ?Bool; OutCharInBoolStream

-- Write and read on an out-char-in-bool stream; return the continuation channel
writeCharReadBool : ∀ β:SL . !Char; ?Bool; β -> β
writeCharReadBool c =
  let (v, c) = receive (send 'z' c) in printBool v; c

writeCharReadBoolStream : OutCharInBoolStream -> ()
writeCharReadBoolStream =
  consumeStream[!Char; ?Bool] writeCharReadBool[OutCharInBoolStream]

-- Read from an out-char-in-bool stream
readCharWriteBool : ∀ β:SL . ?Char; !Bool; β -> β
readCharWriteBool c =
  let (v, c) = receive c in
  printChar v; send False c

readCharWriteBoolStream : dualof OutCharInBoolStream -> ()
readCharWriteBoolStream =
  consumeStream[?Char; !Bool] readCharWriteBool[dualof OutCharInBoolStream]

-- Run an out-char-in-bool stream
mainCharBoolStream : ()
mainCharBoolStream =
  let (w, r) = new OutCharInBoolStream in
  fork[()] (writeCharReadBoolStream w);
  readCharWriteBoolStream r
-}
-- Go!
main : ()
main = mainIntStream
