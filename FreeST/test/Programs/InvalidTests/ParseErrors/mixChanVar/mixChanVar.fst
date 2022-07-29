f : &{A: &{C: Skip, D: Skip}, B: Skip} 1-> Int
f (A (C c)) = 0
f (A (D c)) = 1
f (A c) = 1
f c = 2