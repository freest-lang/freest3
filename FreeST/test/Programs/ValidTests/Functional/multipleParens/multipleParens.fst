{- |
Module      :  Parens
Description :  This program tests the associativity of parens
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

parens : Int
parens = (((2+4)-(3-1))+1)+2-(1+6)

main : Int
main = parens

