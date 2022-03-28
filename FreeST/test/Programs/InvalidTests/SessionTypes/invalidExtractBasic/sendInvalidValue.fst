
-- It doesn't really matter, it should not throw an error here
main : Int
main = 10

data Tree = Leaf | Node Int Tree Tree

-- invalid extract pair: Expecting a pair type; found Bool
fun : !Int;Skip -> Tree -> Bool
fun c t = let x = send t c in True
