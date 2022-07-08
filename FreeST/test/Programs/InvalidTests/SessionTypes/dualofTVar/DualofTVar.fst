myNew : ∀a: 1S . (a, dualof a)
myNew = new a

run : a: 1S . (a -> ()) 1-> (dualof a -> ()) 1-> ()
run f g = let (x, y) = myNew @a in fork @() (f x); g y

write : !Int -> ()
write c = let _ = send 5 c in ()

read : ?Int -> ()
read c = let (_, _) = receive c in ()

main : ()
main = run  @(!Int) write read
