{- |
Module      :  MultDiv Precedence
Description :  This program tests the precedence of divisions and multiplications over sum
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}


start : Int
start = precedence

precedence : Int
precedence = div 6 2 * (1 + 2)
