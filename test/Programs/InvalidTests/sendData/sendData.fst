data Tree = Leaf | Node Int Tree Tree

f : Char
f t = send (Node 5 Leaf Leaf)

main : Int
main = 5
