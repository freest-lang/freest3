{- |
Module      :  ArithExprServer
Description :  Server that computes arithmetic expressions
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos

This example is from Thiemann and Vasconcelos:
"Context-Free Session Types" (listing 3)

The interaction with this server is performed by sending
an arithmetic expression. The server reads the expression,
computes its value and returns the value on the same channel.

This version uses the pipeline operator |>.

-}

type TermChannel : SL  = +{
   Const: !Int,
   Add: TermChannel;TermChannel,
   Mult: TermChannel;TermChannel
 }

-- Read an arithmetic expression from a channel; compute its value;
-- return the value on the same channel.
computeService : dualof TermChannel;!Int -> ()
computeService c =
  let (n1, c1) = receiveEval[!Int;Skip] c in
  send n1 c1 &
  (\_:Skip -> ())

-- Read an arithmetic expression in the front of a channel; compute
-- its value; return the pair composed of this value and the channel
-- residual.
receiveEval : forall a:SL . dualof TermChannel;a -> (Int, a)
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
client c = select Add c
        & select Const
        & send 5
        & select Mult
        & select Const
        & send 7
        & select Const
        & send 9
        & receive
        & fst[Int][Skip]

main : Int
main =
  let (w, r)  = new dualof TermChannel;!Int in
  fork[()] (computeService w);
  client r
