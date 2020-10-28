{-
I wish we could write

type Stream α:SL = α; Stream[α]

or

type Stream α:SL = rec b: SL. α;b

or

type Stream = forall α:SL => rec b: SL. α;b

Note: this is not exactly a stream for α can be instantiated with
!Char ; ?Bool forcing data to flow on both directions.
-}

-- An arbitrary Stream consumer
consumeStream : forall α:SL =>
  (α;(rec b: SL. α;b) -> (rec b: SL. α;b)) ->   -- A function that consumes the head of a stream
  (rec b: SL. α;b) ->                           -- The stream
  ()
consumeStream f c = consumeStream[α] f (f c)

-- 1 _ Stream of integer values
type OutIntStream : SL = !Int; OutIntStream

-- Write on an int on a channel; return the continuation channel
writeInt : forall β:SL => !Int; β -> β
writeInt c = send 7 c

writeIntStream : OutIntStream -> ()
writeIntStream = consumeStream[!Int] writeInt[OutIntStream]

-- Read from an int stream
readInt : forall β:SL => ?Int; β -> β
readInt c = let (v, c) = receive c in printInt v; c

readIntStream : dualof OutIntStream -> ()
readIntStream = consumeStream[?Int] readInt[dualof OutIntStream]

-- Run an int stream
mainIntStream : ()
mainIntStream =
  let (w, r) = new OutIntStream in
  fork (writeIntStream w);
  readIntStream r

-- 2 _ Stream of out-char-in-bool values
type OutCharInBoolStream : SL = !Char; ?Bool; OutCharInBoolStream

-- Write and read on an out-char-in-bool stream; return the continuation channel
writeCharReadBool : forall β:SL => !Char; ?Bool; β -> β
writeCharReadBool c =
  let (v, c) = receive (send 'z' c) in printBool v; c

writeCharReadBoolStream : OutCharInBoolStream -> ()
writeCharReadBoolStream =
  consumeStream[!Char; ?Bool] writeCharReadBool[OutCharInBoolStream]

-- Read from an out-char-in-bool stream
readCharWriteBool : forall β:SL => ?Char; !Bool; β -> β
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
  fork (writeCharReadBoolStream w);
  readCharWriteBoolStream r

-- Go!
main : ()
-- main = mainIntStream
main = mainCharBoolStream
