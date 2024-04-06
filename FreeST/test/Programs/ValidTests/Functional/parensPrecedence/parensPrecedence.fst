{- |
Module      :  ParensPrecedence
Description :  This program tests the order of the operations with parens
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

precedence : Int
precedence = ((2 + 3) * 4) * (1 + 5)

main : Int
main = precedence

