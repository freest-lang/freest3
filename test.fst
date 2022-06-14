
data T = A Int | B

f : T -> T
f (A i) = B
f (A i) = A i
f B = A 1

main : T
main = f B