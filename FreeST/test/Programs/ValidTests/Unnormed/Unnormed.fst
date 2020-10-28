{- |
Module      :  Unnormed
Description :  An infinite interaction
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos

Note: In the web version, infinite programs that **print** values to
stdout, like this one, only show results after a system defined
timeout.

The writer is expected to write recursively and in the end read as
many times as were written on the channel.  In practice, interactions
on a channel described by T are interactions in which the writer
writes infinitely, because it is impossible to reach the state where
the writer supposedly reads the integers back from the channel.

FreeST builds on a novel algorithm for deciding type equivalence of
context-free session types. (see the algorithm at Almeida B., Mordido
A., Vasconcelos V.T. "Deciding the bisimilarity of context-free
session types").

The type !Int;T;?Int is equivalent to the type !Int;T because the
unreachable sequences are pruned during the algorithm that decides
whether two context-free session types are equivalent or not.

-}

type T : SL = !Int;T;?Int

writer : Int -> T -> Skip
writer i c = writer (i + 1) (send i c)

reader : dualof T -> ()
reader c =
  let (i, c) = receive c in
  printIntLn i;
  reader c

main : ()
main =
  let (w, r) = new T in
  let _ = fork (sink (writer 0 w)) in
  reader r

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
