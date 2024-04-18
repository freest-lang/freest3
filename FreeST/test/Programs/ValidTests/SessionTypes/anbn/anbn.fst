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
type S0 = +{A: S1}
-- Production S1
type S1 = +{A: S1; +{B: Skip}, B: Skip}

-- The client selects a given number of A's
client : Int -> S0;Close -> ()
client n c = c |> select A |> client' @Close (n - 1) |> close

-- for each A selected a B is also selected
client' : Int -> S1;a -> a
client' n c =
  if n == 0
  then
    select B c                                  -- a
  else
    let c = select A c in                       -- S1; +{B: Skip}; a
    let c = client' @(+{B: Skip} ; a) (n - 1) c in -- +{B: Skip}; a
    select B c                                  -- a

-- The server offers the choice composed by A
server : dualof S0;Wait -> ()
server c =
  match c with {
    A c -> c |> server' @Wait |> wait
  }

-- For each A selected, a choice for B is also offered
server' : dualof S1; a -> a
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

main : ()
main =
  let (w, r) = new @(S0;Close) () in
  fork @() (\_:()1-> client 25 w);
  server r
