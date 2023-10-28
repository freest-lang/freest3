{- |
Module      :  ArithExprServer
Description :  Server that computes arithmetic expressions
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos

This example is from Thiemann and Vasconcelos:
"Context-Free Session Types" (listing 3)

The interaction with this server is performed by sending
an arithmetic expression. The server reads the expression,
computes its value and returns the value on the same channel.

-}

type TermChannel : 1S  = +{
   Const: !Int,
   Add: TermChannel;TermChannel,
   Mult: TermChannel;TermChannel
 }

-- Read an arithmetic expression from a channel; compute its value;
-- return the value on the same channel.
computeService : dualof TermChannel ; !Int ; EndC -> ()
computeService c =
  let (n1, c1) = receiveEval @(!Int ; EndC) c in
  c1 |> send n1 |> close

-- Read an arithmetic expression in the front of a channel; compute
-- its value; return the pair composed of this value and the channel
-- residual.
receiveEval : forall a: 1S . dualof TermChannel;a -> (Int, a)
receiveEval c =
  match c with {
    Const c ->
      receive c,
    Add c ->
      let (n1, c) = receiveEval @(dualof TermChannel ; a) c in
      let (n2, c) = receiveEval @a c in
      (n1 + n2, c),
    Mult c ->
      let (n1, c) = receiveEval @(dualof TermChannel ; a) c in
      let (n2, c) = receiveEval @a c in
      (n1 * n2, c)
  }

-- Compute 5 + (7 * 9); return the result
client : TermChannel ; ?Int ; EndW -> Int
client c = c |> select Add 
             |> select Const 
             |> send 5 
             |> select Mult 
             |> select Const 
             |> send 7
             |> select Const 
             |> send 9 
             |> receiveAndWait @Int

main : Int
main =
  let (w, r) = new @(dualof TermChannel ; !Int ; EndC) () in
  fork @() (\_:() 1-> computeService w);
  client r
