
data T = Nil | Cons Int List

f : T -> T -> Int
f (Cons x (Cons y Cons xs)) xs = x