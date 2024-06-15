{- |
Module      :  MultPrecedence
Description :  This program tests the precedence of a multiplication over sum
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

precedence : Int
precedence = 2 + 3 * 4 + 5

main : Int
main = precedence

