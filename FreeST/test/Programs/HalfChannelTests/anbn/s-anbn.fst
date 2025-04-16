{- |
Module      :  anbn
Description :  The context-free language {A^nB^n | n >= 1}
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos

The language of S0 is {A^nB^n | n >= 1}

S0 -> a S1
S1 -> a S1 b | b

The language generated from S0 is not regular.

This example is from unpublished notes by Frank Pfenning and Henry DeYoung
on a simplified representation of deterministic pushdown automata.
-}

-- Production S0
type S0 = &{A: S1}
-- Production S1
type S1 = &{A: S1; &{B: Skip}, B: Skip}

-- For each A selected, a choice for B is also offered
server' : S1; a -> a
server' c =
  match c with {
    A c ->     -- (rec x . &{A: x; &{B: Skip}, B: Skip})) ; &{B: Skip}
      (let c = server' @(&{B: Skip};a) c in  -- &{B: Skip}; a
       match c with {
         B c -> c
       }),                                  -- a
    B c ->                                  -- a
      c
  }

-- The server offers the choice composed by A
server : S0;Wait -> ()
server c =
  match c with {
    A c -> c |> server' @Wait |> wait
  }

main : ()
main =
  newHcServer @(S0; Wait) ("127.0.0.1", "8081") |>
  server ; 
  ()
