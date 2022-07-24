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
consumeStream f c = consumeStream @α f (f c)

-- 1 _ Stream of integer values
type OutIntStream : 1S = !Int; OutIntStream

-- Write on an int on a channel; return the continuation channel
writeInt : forall β: 1S . !Int; β -> β
writeInt c = send 7 c

writeIntStream : OutIntStream -> ()
writeIntStream = consumeStream @(!Int) writeInt @OutIntStream

-- Read from an int stream
readInt : forall β: 1S . ?Int; β -> β
readInt c = let (v, c) = receive c in printInt v; c

readIntStream : dualof OutIntStream -> ()
readIntStream = consumeStream @(?Int) readInt @(dualof OutIntStream)

-- Run an int stream
mainIntStream : ()
mainIntStream =
  let (w, r) = new OutIntStream in
  fork @() (\_:() 1-> writeIntStream w);
  readIntStream r

-- 2 _ Stream of out-char-in-bool values
type OutCharInBoolStream : 1S = !Char; ?Bool; OutCharInBoolStream

-- Write and read on an out-char-in-bool stream; return the continuation channel
writeCharReadBool : forall β: 1S . !Char; ?Bool; β -> β
writeCharReadBool c =
  let (v, c) = receive (send 'z' c) in printBool v; c

writeCharReadBoolStream : OutCharInBoolStream -> ()
writeCharReadBoolStream =
  consumeStream @(!Char ; ?Bool) writeCharReadBool @OutCharInBoolStream

-- 1 _ Stream of integer values
type IntStream: 1S = !Int; IntStream

writeIntStream : IntStream -> ()
writeIntStream = produceStream  @Int (\c:IntStream -> send 7 c)

-- Run an out-char-in-bool stream
mainCharBoolStream : ()
mainCharBoolStream =
  let (w, r) = new OutCharInBoolStream in
  fork @() (\_:() 1-> writeCharReadBoolStream w);
  readCharWriteBoolStream r

main : ()
main =
  let (w, r) = new IntStream in
  fork @() (\_:() 1-> writeIntStream w);
  readIntStream r
