
data T = A | B | C T | D T

f : T -> Int
f (D (D A)) = 0
f (D (D x)) = 1

main : Int
main = f $ D $ D $ B

-- a mixture tem de ser puxada para trás