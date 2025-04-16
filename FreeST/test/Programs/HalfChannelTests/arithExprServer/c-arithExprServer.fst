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

type TermChannel  = +{
   Const: !Int,
   Add: TermChannel;TermChannel,
   Mult: TermChannel;TermChannel
 }

-- Compute 5 + (7 * 9); return the result
client : TermChannel ; ?Int ; Wait -> Int
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
  newHcClient1 @(TermChannel ; ?Int ; Wait) ("127.0.0.1", "8081") |>
  client
 