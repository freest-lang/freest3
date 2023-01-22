type MathServer : 1S = &{ Val : ?Bool; MathServer
                        , And : !Bool; MathServer
                        , Not : !Bool; MathServer
                        , Quit: End
                        }
type MathClient : 1S = dualof MathServer

-- Initializes mathServer with a default boolean tuple
initMathServer : MathServer -> ()
initMathServer c = mathServer False False c

-- While receiving values, the pair b1 and b2 function as a "partial stack"
--   storing only the last 2 values, where b1 is the "top" of the stack.
mathServer : Bool -> Bool -> MathServer -> ()
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

    Quit c -> close c
  }


-- ==================== Client ====================

-- Sends False, then True, then True, and calls for And
--   should return True
client : MathClient -> Bool
client c =
  let (b, c) = 
    sendBool False c 
    |> sendBool True
    |> sendBool True
    |> select And
    |> receive in
  select Quit c |> close; 
  b


-- ==================== Aux Functions ====================

sendBool : Bool -> MathClient 1-> MathClient
sendBool b c =
  select Val c
  |> send b


-- ==================== Main ====================
main : Bool
main =
  let (r, w) = new @MathServer () in
  fork @() $ initMathServer r;
  client w
