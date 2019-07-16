{-

type TermChannel = +{
  Const: !Int,
  Add: TermChannel;TermChannel,
  Mult: TermChannel;TermChan
}

-}

-- Read an arithmetic expression from a channel; compute its value;
-- return the value on the same channel.
computeService : (rec x:SL. &{Const: ?Int, Add: x;x, Mult: x;x});!Int -> Skip
computeService c =
  let n1, c1 = receiveEval[!Int;Skip] c in
  send c1 n1

-- Read an arithmetic expression in the front of a channel; compute
-- its value; return the pair composed of this value and the channel
-- residual.
receiveEval : forall α:SL => (rec x:SL. &{Const: ?Int, Add: x;x, Mult: x;x});α -> (Int, α)
receiveEval c =
  match c with {
    Const c ->
      receive c,
    Add c ->
      let n1, c = receiveEval[(rec termChan:SL. &{Const: ?Int, Add: termChan;termChan, Mult: termChan;termChan});α] c in
      let n2, c = receiveEval[α] c in
      (n1 + n2, c),
    Mult c ->
      let n1, c = receiveEval[(rec termChan:SL. &{Const: ?Int, Add: termChan;termChan, Mult: termChan;termChan});α] c in
      let n2, c = receiveEval[α] c in
      (n1 * n2, c)
  }

-- Compute 5 + (7 * 9); return the result
client : (rec x:SL. +{Const: !Int, Add: x;x, Mult: x;x});?Int -> Int
client c =
  let c = select c Add in
  let c = select c Const in
  let c = send c 5 in
  let c = select c Mult in
  let c = select c Const in
  let c = send c 7 in
  let c = select c Const in
  let c = send c 9 in
  let n, _ = receive c in
  n

main : Int
main =
  let w, r  = new (rec x:SL . &{Const: ?Int, Add: x;x, Mult: x;x});!Int in
  let _ = fork (computeService w) in
  client r
