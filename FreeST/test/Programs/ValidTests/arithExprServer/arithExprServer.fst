{- |
Module      :  ArithExprServer
Description :  Server that computes arithmetic expressions
Copyright   :  (c) Bernardo Almeida, Vasco T. Vasconcelos, Andreia Mordido

This example is from Thiemann and Vasconcelos:
"Context-Free Session Types" (listing 3)

The interaction with this server is performed by sending
an arithmetic expression. The server reads the expression,
computes its value and returns the value on the same channel.

-}


type TermChannel : SL  = +{
   Const: !Int,
   Add: TermChannel;TermChannel,
   Mult: TermChannel;TermChannel
 }



-- Read an arithmetic expression from a channel; compute its value;
-- return the value on the same channel.
computeService : dualof TermChannel;!Int -> Skip
computeService c =
  let (n1, c1) = receiveEval[!Int;Skip] c in
  send c1 n1

-- Read an arithmetic expression in the front of a channel; compute
-- its value; return the pair composed of this value and the channel
-- residual.
receiveEval : forall a:SL => dualof TermChannel;a -> (Int, a)
receiveEval c =
  match c with {
    Const c ->
      receive c,
    Add c ->
      let (n1, c) = receiveEval[dualof TermChannel;a] c in
      let (n2, c) = receiveEval[a] c in
      (n1 + n2, c),
    Mult c ->
      let (n1, c) = receiveEval[dualof TermChannel;a] c in
      let (n2, c) = receiveEval[a] c in
      (n1 * n2, c)
  }

-- Compute 5 + (7 * 9); return the result
client : TermChannel;?Int -> Int
client c =
  let c = select c Add in
  let c = select c Const in
  let c = send c 5 in
  let c = select c Mult in
  let c = select c Const in
  let c = send c 7 in
  let c = select c Const in
  let c = send c 9 in
  let (n, _) = receive c in
  n

main : Int
main =
  let (w, r)  = new dualof TermChannel;!Int in
  let _ = fork (computeService w) in
  client r
