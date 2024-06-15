{- |
Module      :  Test a simple application
Description :  Test s Application (test a function application with simple application as an argument
               without parens in order to test the operator precedence)
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

half : Int -> Int
half x = div x 2

main : Int
main = half (2+2*3)


