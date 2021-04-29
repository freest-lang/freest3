-- Initializes mathServer with a default boolean tuple
initMathServer : (rec x: SL. &{Val: ?Bool; x, And: !Bool; x, Not: !Bool; x, Quit: Skip}) -> ()
initMathServer c = mathServer False False c

-- While receiving values, the pair b1 and b2 function as a "partial stack"
--   storing only the last 2 values, where b1 is the "top" of the stack.
mathServer : Bool -> Bool -> (rec x: SL. &{Val: ?Bool; x, And: !Bool; x, Not: !Bool; x, Quit: Skip}) -> ()
mathServer b1 b2 c =
  match c with {
    Val c ->
      let (v, c)  = receive c in
      mathServer v b1 c,

    And c ->
      let c = send (b1 && b2) c in
      mathServer b1 b2 c,

    Not c ->
      let c = send (not b1) c in
      mathServer b1 b2 c,

    Quit c ->
      ()
  }


-- ==================== Client ====================

-- Sends False, then True, then True, and calls for And
--   should return True
client : (rec x: SL. +{Val: !Bool; x, And: ?Bool; x, Not: ?Bool; x, Quit: Skip}) -> Bool
client c =
  let c = sendBool c False in
  let c = sendBool c True in
  let c = sendBool c True in
  let c = select And c in
  let (b, c) = receive c in
  let c = select Quit c in
  b


-- ==================== Aux Functions ====================

sendBool : (rec x: SL. +{Val: !Bool; x, And: ?Bool; x, Not: ?Bool; x, Quit: Skip}) -> Bool -o (rec x: SL. +{Val: !Bool; x, And: ?Bool; x, Not: ?Bool; x, Quit: Skip})
sendBool c b =
  let c = select Val c in
  let c = send b c in
  c


-- ==================== Main ====================
main : Bool
main =
  let (r, w) = new rec x: SL. &{Val: ?Bool; x, And: !Bool; x, Not: !Bool; x, Quit: Skip} in
  let _      = fork[()] $ initMathServer r in
  client w
