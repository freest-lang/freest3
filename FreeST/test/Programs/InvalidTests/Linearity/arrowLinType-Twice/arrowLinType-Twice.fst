type Arrow : 1T = Int 1-> Bool

is10 : Arrow
is10 x = x == 10



main : Bool
main = is10 10 || is10 12
