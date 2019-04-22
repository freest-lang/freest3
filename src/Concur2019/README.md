#   Deciding the bisimilarity of context-free session types

Bernardo Almeida

Andreia Mordido

Vasco T. Vasconcelos

University of Lisbon

This folder contains the source code for the algorithm described in the paper. The two packages, `Syntax` and `Equivalence`, are taken from the compiler for the FreeST language.

1. Function `toGrammar` (Listing 1 of the paper) is in module `Equivalence.TypeToGrammar`

2. Function `prune` (Listing 2) is in module `Equivalence.Norm`

3. Function `bisimilar` (Listing 3) is in module `Equivalence.Bisimulation`

Script `Main.hs` runs the examples in the paper.
