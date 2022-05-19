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

Example suggested by the authors Frank Pfenning, Ankush Das, Henry DeYoung, and Andreia Mordido.

-}

type D : 1S = +{ Lt : T;D, Dollar : Skip }
type T : 1S = +{ Lt : T;T, Gt : Skip }

-- Read from a channel; print what is read
readD : forall a: 1S . dualof D;a -> a
readD c =
  match c with {
    Lt c ->
      printChar '<';
      readD[a] (readT[dualof D;a] c),
    Dollar c ->
      printCharLn '$';
      c
  }
readT : forall a: 1S . dualof T;a -> a
readT c =
  match c with {
    Lt c ->
      printChar '<';
      readT[a] (readT[dualof T;a] c),
    Gt c ->
      printChar '>';
      c
  }

-- Read from a channel and immediately write on another channel
forwardD : forall a: 1S . forall b: 1S . dualof D;a -> D;b -o (a, b)
forwardD in' out =
  match in' with {
    Lt in' ->
      let out = select Lt out in
      let (in', out) = forwardT[dualof D;a][D;b] in' out in
        forwardD[a][b] in' out,
    Dollar in' ->
      let out = select Dollar out in
         (in', out)
  }

forwardT : forall a: 1S . forall b: 1S . dualof T;a -> T;b -o (a, b)
forwardT in' out =
  match in' with {
    Lt in' ->
      let out = select Lt out in
      let (in', out) = forwardT[dualof T;a][T;b] in' out in
      forwardT[a][b] in' out,
    Gt in' ->
      let out = select Gt out in
      (in', out)
  }

-- Read from a channel; read from a second channel; while writing on a
-- third channel
concatD : forall a: 1S . forall b: 1S . forall c: 1S . dualof D;a -> dualof D;b -o D;c -o (a, (b, c))
concatD in1 in2 out =
  match in1 with {
    Lt in1 ->
      let out = select Lt out in
      let (in1, in2out) = concatT[dualof D;a][dualof D;b][D;c] in1 in2 out in
      let (in2, out) = in2out in
        concatD[a][b][c] in1 in2 out,
    Dollar in1 ->
      let (in2, out) = forwardD[b][c] in2 out in
         (in1, (in2, out))
  } -- forwardD : forall a: 1S . forall b: 1S . dualof D;a -> D;b -> (a, b)

concatT : forall a: 1S . forall b: 1S . forall c: 1S . dualof T;a -> b -o T;c -o (a, (b, c))
concatT in1 in2 out =
  match in1 with {
    Lt in1 ->
      let out = select Lt out in
      let (in1, in2out) = concatT[dualof T;a][b][T;c] in1 in2 out in
      let (in2, out) = in2out in
      concatT[a][b][c] in1 in2 out,
    Gt in1 ->
      let out = select Gt out in
      (in1, (in2, out))
  }

-- A few functions to write on channels
writeLtGt : D -> Skip
writeLtGt c =
  select Dollar $ select Gt $ select Lt c

writeDollar : D -> Skip
writeDollar c = select Dollar c

writeLtLtGtGtLtGt : D -> Skip
writeLtLtGtGtLtGt c =
  select Dollar $ select Gt $ select Lt $ select Gt $ select Gt $ select Lt $ select Lt c

writeLtLtGtLtGtGt: D -> Skip
writeLtLtGtLtGtGt c =
  select Dollar $ select Gt $ select Gt $ select Lt $ select Gt $ select Lt $ select Lt c

-- Putting it all together: out1 -> in1-out2 --> in2
mainForward : Skip
mainForward =
  let (out1, in1) = new D in
  let (out2, in2) = new D in
  fork[Skip] $ writeLtLtGtGtLtGt out1;
  fork[(Skip, Skip)] (forwardD[Skip][Skip] in1 out2);
  readD[Skip] in2

-- Putting it all together: (out1 | out2) --> in1-in2-out3 --> in3
main : Skip
main =
  let (out1, in1) = new D in
  let (out2, in2) = new D in
  let (out3, in3) = new D in
  fork[Skip] (writeLtLtGtGtLtGt out1);
  fork[Skip] (writeLtLtGtLtGtGt out2);
  fork[(Skip, Skip, Skip)] (concatD[Skip][Skip][Skip] in1 in2 out3);
  readD[Skip] in3
