
type Arrow : 1T = Int 1-> Bool

isTen : Arrow
isTen x = x == 10

main : Bool
main = isTen 10
