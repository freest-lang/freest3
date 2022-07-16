f : !(∀a . a -> a) -> Skip
f c = send id c

g : !(∀b . b -> b) -> Skip
g = f

main : Int
main = 5
  

