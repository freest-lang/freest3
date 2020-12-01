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

type D : SL = +{ Lt : T;D, Dollar : Skip }
type T : SL = +{ Lt : T;T, Gt : Skip }

-- Read from a channel; print what is read
readD : forall a:SL => dualof D;a -> a
readD c =
  match c with {
    Lt c ->
      printChar '<';
      readD[a] (readT[dualof D;a] c),
    Dollar c ->
      printCharLn '$';
      c
  }
readT : forall a:SL => dualof T;a -> a
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
forwardD : forall a:SL => forall b:SL => dualof D;a -> D;b -> (a, b)
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

forwardT : forall a:SL => forall b:SL => dualof T;a -> T;b -> (a, b)
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
concatD : forall a:SL => forall b:SL => forall c:SL => dualof D;a -> dualof D;b -> D;c -> (a, (b, c))
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
  }

concatT : forall a:SL => forall b:SL => forall c:SL => dualof T;a -> b -> T;c -> (a, (b, c))
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
  fork (sink[Skip] $ writeLtLtGtGtLtGt out1);
  fork (sink[(Skip, Skip)] (forwardD[Skip][Skip] in1 out2));
  readD[Skip] in2

-- Putting it all together: (out1 | out2) --> in1-in2-out3 --> in3
main : Skip
main =
  let (out1, in1) = new D in
  let (out2, in2) = new D in
  let (out3, in3) = new D in
  fork (sink[Skip] (writeLtLtGtGtLtGt out1));
  fork (sink[Skip] (writeLtLtGtLtGtGt out2));
  fork (sink[(Skip, Skip, Skip)] (concatD[Skip][Skip][Skip] in1 in2 out3));
  readD[Skip] in3

-- To be used with fork : () -> ()
sink : forall a : SU => a -> ()
sink _ = ()

