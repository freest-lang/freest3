
data T = A | B | C T | D T

f : T -> T -> Int
f (C (C A)) A = 0
f (C (C x)) B = 1
f (C (C B)) x = 2
f (C (C B)) x = 3
f (D (D x)) (D A) = 4
f x y = 5

main : Int
main = f (C $ C $ B) (D A)
