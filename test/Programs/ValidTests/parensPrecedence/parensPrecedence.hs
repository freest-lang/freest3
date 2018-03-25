{- |
Module      :  ParensPrecedence
Description :  This program tests the order of the operations with parens
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

start :: Int
start = precedence

precedence :: Int
precedence = ((2 + 3) * 4) * (1 + 5)
