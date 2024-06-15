myNew : () -> (a, dualof a)
myNew _ = new @a ()

main : (Skip, Skip)
main = myNew @Skip ()
