
data T = A | B

f : T -> T
f A = B
f B = A

main : T
main = f A