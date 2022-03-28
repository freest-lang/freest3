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
type S0 : SL = +{A: S1}
-- Production S1
type S1 : SL = +{A: S1; +{B: Skip}, B: Skip}

-- The client selects a given number of A's
client : Int -> S0 -> ()
client n c =
  let c = select A c in
  let _ = client'[Skip] (n - 1) c in
  ()

-- for each A selected a B is also selected
client' : forall a : SL . Int -> S1;a -> a
client' n c =
  if n == 0
  then
    select B c                                  -- a
  else
    let c = select A c in                       -- S1; +{B: Skip}; a
    let c = client'[+{B: Skip}; a] (n - 1) c in -- +{B: Skip}; a
    select B c                                  -- a

-- The server offers the choice composed by A
server : dualof S0 -> Skip
server c =
  match c with {
    A c -> server'[Skip] c
  }

-- For each A selected, a choice for B is also offered
server' : forall a : SL . dualof S1; a -> a
server' c =
  match c with {
    A c ->     -- (rec x:SL. &{A: x; &{B: Skip}, B: Skip})) ; &{B: Skip}
      (let c = server'[&{B: Skip}; a] c in  -- &{B: Skip}; a
       match c with {
         B c -> c
       }),                                  -- a
    B c ->                                  -- a
      c
  }

main : Skip
main =
  let (w, r) = new S0 in
  fork[()] $ client 25 w;
  server r
