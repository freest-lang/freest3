{- |
Module      :  DyckWords
Description :  An implementation of a Dyck language
Copyright   :  (c) Vasco T. Vasconcelos

The variant of a dyck word implemented is a balanced string of '<''>'
delimiters.

The context-free grammar that generates the dyck language is generated by a
single non-terminal S, and the following production:

S -> e | "[" S "]" S

(where e stands for epsilon-productions)

Example suggested by Frank Pfenning, Ankush Das, Henry DeYoung, and Andreia Mordido.

-}

type D = +{ Lt : T;D, Dollar : Skip }
type T = +{ Lt : T;T, Gt : Skip }

-- Read from a channel; print what is read
readT : dualof T;a -> a
readT c =
  match c with {
    Lt c ->
      putStr (show @Char '<');
      readT @a (readT @(dualof T ; a) c),
    Gt c ->
      putStr (show @Char '>');
      c
  }

readD : dualof D;a -> a
readD c =
  match c with {
    Lt c ->
      print @Char '<';
      readD @a (readT @(dualof D ; a) c),
    Dollar c ->
      putStr (show  @Char '$');
      c
  }

forwardT : dualof T;a -> T;b 1-> (a, b)
forwardT in' out =
  match in' with {
    Lt in' ->
      let out = select Lt out in
      let (in', out) = forwardT @(dualof T ; a) @(T ; b) in' out in
      forwardT @a @b in' out,
    Gt in' ->
      let out = select Gt out in
      (in', out)
  }

-- Read from a channel and immediately write on another channel
forwardD : dualof D;a -> D;b 1-> (a, b)
forwardD in' out =
  match in' with {
    Lt in' ->
      let out = select Lt out in
      let (in', out) = forwardT @(dualof D ; a) @(D ; b) in' out in
        forwardD @a @b in' out,
    Dollar in' ->
      let out = select Dollar out in
         (in', out)
  }

concatT : dualof T;a -> b 1-> T;c 1-> (a, (b, c))
concatT in1 in2 out =
  match in1 with {
    Lt in1 ->
      let out = select Lt out in
      let (in1, in2out) = concatT @(dualof T ; a) @b @(T ; c) in1 in2 out in
      let (in2, out) = in2out in
      concatT @a @b @c in1 in2 out,
    Gt in1 ->
      let out = select Gt out in
      (in1, (in2, out))
  }

-- Read from a channel; read from a second channel; while writing on a
-- third channel
concatD : dualof D;a -> dualof D;b 1-> D;c 1-> (a, (b, c))
concatD in1 in2 out =
  match in1 with {
    Lt in1 ->
      let out = select Lt out in
      let (in1, in2out) = concatT @(dualof D ; a) @(dualof D ; b) @(D ; c) in1 in2 out in
      let (in2, out) = in2out in
        concatD @a @b @c in1 in2 out,
    Dollar in1 ->
      let (in2, out) = forwardD @b @c in2 out in
         (in1, (in2, out))
  } -- forwardD : forall a . forall b . dualof D;a -> D;b -> (a, b)

-- A few functions to write on channels
writeLtGt, writeDollar, writeLtLtGtGtLtGt, writeLtLtGtLtGtGt : D;Close -> Close

writeLtGt c =
  select Dollar $ select Gt $ select Lt c

writeDollar c = select Dollar c

writeLtLtGtGtLtGt c =
  select Dollar $ select Gt $ select Lt $ select Gt $ select Gt $ select Lt $ select Lt c

writeLtLtGtLtGtGt c =
  select Dollar $ select Gt $ select Gt $ select Lt $ select Gt $ select Lt $ select Lt c

-- Putting it all together: out1 -> in1-out2 --> in2
mainForward : ()
mainForward =
  let (out1, in1) = new @(D;Close) () in
  let (out2, in2) = new @(D;Close) () in
  fork @() (\_:()1-> writeLtLtGtGtLtGt out1 |> close);
  fork @() (\_:()1-> let (c1, c2) = forwardD @Wait @Close in1 out2 in wait c1; close c2);
  readD @Wait in2 |> wait

-- Putting it all together: (out1 | out2) --> in1-in2-out3 --> in3
main : ()
main =
  let (out1, in1) = new @(D;Close) () in
  let (out2, in2) = new @(D;Close) () in
  let (out3, in3) = new @(D;Close) () in
  fork @() (\_:()1-> writeLtLtGtGtLtGt out1 |> close);
  fork @() (\_:()1-> writeLtLtGtLtGtGt out2 |> close);
  fork @() (\_:()1-> 
    let (c1, c23) = concatD @Wait @Wait @Close in1 in2 out3 in 
    let (c2, c3 ) = c23 in 
    wait c1; wait c2; close c3);
  readD @Wait in3 |> wait
